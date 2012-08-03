;;; org-caldav.el --- Sync org files with external calendar through CalDAV

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author: David Engster <dengste@eml.cc>
;; Keywords: calendar, caldav
;;
;; This file is not part of GNU Emacs.
;;
;; org-caldav.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; org-caldav.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; Tested CalDAV servers: Owncloud, Google Calendar.
;;
;; IMPORTANT: Do NOT simply put your main calendar in `org-caldav-calendar-id'!
;;
;; Instead, create a new, dedicated calendar.  The code is still
;; pretty rough and might easily delete entries it should not delete.
;;
;; This package depends on the url-dav package, which unfortunately is
;; broken in Emacs proper. Get a fixed one from
;;   https://github.com/dengste/org-caldav
;; and load it before using org-caldav.
;;
;; In a nutshell:
;;
;; - Create a new calendar; the name does not matter. Again, do *not*
;;   use your precious main calendar.
;;
;; - Set `org-caldav-url' to the base address of your CalDAV server:
;;    * Owncloud: https://OWNCLOUD-SERVER-URL/remote.php/caldav/calendars/USERID
;;    * Google: https://www.google.com/calendar/dav
;;
;; - Set `org-caldav-calendar-id' to the calendar-id of your new calendar:
;;    * OwnCloud: Simply the name of the calendar.
;;    * Google: Click on 'calendar settings' and the id will be shown
;;      next to "Calendar Address". It is of the form
;;      ID@group.calendar.google.com. Do *not* omit the domain.
;;
;; - Set `org-caldav-files' to the list of org files you would like to
;;   sync. For everything else, you can use the org-icalendar-*
;;   variables, since org-caldav uses that package to generate the
;;   events.
;;
;; - Set `org-caldav-inbox' to an org filename where new entries from
;;   the calendar should be stored.
;;
;; Call `org-caldav-sync' to start the sync. The URL package will ask
;; you for username/password for accessing the calendar.


;;; Code:

(require 'url-dav)
(require 'org-icalendar)
(require 'icalendar)

(defvar org-caldav-url "https://www.google.com/calendar/dav"
  "Base URL for CalDAV access.")

(defvar org-caldav-calendar-id "abcde1234@group.calendar.google.com"
  "ID of your calendar.")

(defvar org-caldav-files '("~/org/appointments.org")
  "List of files which should end up in calendar.
Do NOT put `org-caldav-inbox' in here or you'll get duplicate
entries.")

(defvar org-caldav-inbox "~/org/from-calendar.org"
  "Filename for putting new entries obtained from calendar.")

(defvar org-caldav-debug t)
(defvar org-caldav-debug-buffer "*org-caldav-debug*")

;; Variables users usually should not have to touch

(defvar org-caldav-id-string "-orgmodecaldav"
  "String appended to UID to identify entries which are managed
  by org-caldav.")

;; Internal cache variables

(defvar org-caldav-event-list nil
  "Current event list.
'nil' means it has to be updated.
'empty if there are no events.")

(defvar org-caldav-calendar-preamble nil)

(defun org-caldav-check-connection ()
  "Check connection by doing a PROPFIND on CalDAV URL."
  (org-caldav-debug-print (format "Check connection - doing a PROPFIND on %s."
				  (org-caldav-events-url)))
  (let ((output (url-dav-request (org-caldav-events-url) "PROPFIND" nil nil 1)))
  (unless (eq (plist-get (cdar output) 'DAV:status) 200)
    (org-caldav-debug-print "Got error status from PROPFIND: " output)
    (error "Could not query CalDAV URL %s." (org-caldav-events-url))))
  t)

;; This is partly taken out of url-dav.el, written by Bill Perry.

(defun org-caldav-get-icsfiles-from-properties (properties)
  "Return all ics files from PROPERTIES."
  (let (files)
    (while properties
      (let ((url (car (pop properties))))
      (when (string-match "/$" url)
	(setq url (substring url 0 -1)))
      (when (string-match "\\.ics$" url)
	(push url files))))
    files))

(defun org-caldav-get-event-list ()
  "Get list of events from calendar.
Returns '(empty) if there are no events.
Throws an error if connection fails."
  ;; I'd really like to use `url-dav-directory-files' for this, but
  ;; currently it is unable to differentiate between errors and empty
  ;; directories.
  (let ((output (url-dav-get-properties
		 (org-caldav-events-url)
		 '(DAV:resourcetype) 1)))
    (cond
     ((> (length output) 1)
      ;; Everything looks OK - we got a list of "things".
      ;; Get all ics files you can find in there.
      (mapcar (lambda (file)
		(file-name-sans-extension
		 (file-name-nondirectory file)))
	      (org-caldav-get-icsfiles-from-properties output)))
     ((or (null output)
	  (zerop (length output)))
      ;; This is definitely an error.
      (error "Error while getting eventlist from %s." (org-caldav-events-url)))
     ((and (= (length output) 1)
	   (stringp (car-safe (car output))))
      (let ((status (plist-get (cdar output) 'DAV:status)))
	(if (eq status 200)
	    ;; This is an empty directory
	    'empty
	  (if status
	      (error "Error while getting eventlist from %s. Got status code: %d."
		     (org-caldav-events-url) status)
	    (error "Error while getting eventlist from %s."
		   (org-caldav-events-url)))))))))

(defun org-caldav-get-event (uid)
  "Get event with UID from calendar.
Function returns a buffer containing the event."
  (org-caldav-debug-print (format "Getting event UID %s." uid))
  (with-current-buffer
      (url-retrieve-synchronously (concat (org-caldav-events-url) uid ".ics"))
    (goto-char (point-min))
    (if (null (search-forward "BEGIN:VCALENDAR" nil t))
	(error "Could not find valid icalendar.")
      (beginning-of-line)
      (delete-region (point-min) (point))
      (while (re-search-forward "\^M" nil t)
	(replace-match "")))
    (goto-char (point-min))
    (current-buffer)))

(defun org-caldav-put-event (buffer)
  "Add event in BUFFER to calendar.
The filename will be derived from the UID.  The callee must make
sure the event does not already exists.  Only one event may exist
in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
	(let ((uid (org-caldav-get-uid-from-event)))
	  (org-caldav-debug-print (format "Putting event UID %s." uid))
	  (url-dav-save-resource
	   (concat (org-caldav-events-url) uid ".ics")
	   (current-buffer) "text/calendar; charset=UTF-8")))))

(defun org-caldav-delete-event (uid)
  "Delete event UID from calendar."
  (org-caldav-debug-print (format "Deleting event UID %s.\n" uid))
  (url-dav-delete-file (concat (org-caldav-events-url) uid ".ics")))

(defun org-caldav-events-url ()
  "Return URL for events."
  (if (string-match "google\\.com" org-caldav-url)
      (concat org-caldav-url "/" org-caldav-calendar-id "/events/")
    (concat org-caldav-url "/" org-caldav-calendar-id "/")))

(defun org-caldav-create-event-exist-cache ()
  "Create internal cache for events and exist flags."
  (let ((events (org-caldav-current-event-list)))
    (if (eq events 'empty)
	nil
      (mapcar
       'list
       (org-caldav-filter
	(org-caldav-current-event-list)
	(lambda (x)
	  (string-match (concat org-caldav-id-string "$") x)))))))

(defun org-caldav-sync ()
  "Sync Org with calendar."
  (interactive)
  (unless (or (bound-and-true-p url-dav-patched-version)
	      (url-dav-supported-p (org-caldav-events-url)))
    (error "You have to either use Emacs from bzr, or the patched `url-dav' package \
from the org-caldav repository."))
  (org-caldav-debug-print "Started sync.")
  (org-caldav-check-connection)
  (message "Updating event list.")
  (setq org-caldav-event-list nil)
  (let ((events (org-caldav-create-event-exist-cache)))
    (message "Generating ICS file.")
    (with-current-buffer (org-caldav-generate-ics)
      (goto-char (point-min))
      (setq org-caldav-calendar-preamble (org-caldav-get-preamble))
      (while (org-caldav-narrow-next-event)
	(let* ((uid (org-caldav-rewrite-uid))
	       (exists (assoc uid events))
	       (event (buffer-string)))
	(if exists
	    (progn
	      (org-caldav-debug-print
	       (format "UID %s already exists in calendar." uid))
	      ;; Set flag that this event should be kept.
	      (setcdr exists t))
	  (with-temp-buffer
	    (insert org-caldav-calendar-preamble event "END:VCALENDAR\n")
	    (message "Putting event %s." uid)
	    (org-caldav-put-event (current-buffer))))))
      ;; Remove old entries which were apparently changed/deleted in the
      ;; corresponding org file.
      (message "Purging old events.")
      (mapc
       (lambda (x)
	 (org-caldav-delete-event (car x)))
       (org-caldav-filter events (lambda (x) (not (cdr x)))))
    ;; Sync new events to org file
    (org-caldav-insert-new-events)
    (let ((filename (buffer-file-name)))
      (set-buffer-modified-p nil)
      (kill-buffer)
      (delete-file filename))
    (message "Finished sync."))))

(defun org-caldav-insert-new-events ()
  "Insert new events from calendar into `org-caldav-inbox'."
  (with-current-buffer (find-file-noselect org-caldav-inbox)
    (let ((allevents (org-caldav-current-event-list))
	  (caldav-uids (org-caldav-get-all-caldav-uids)))
      (unless (eq allevents 'empty)
	(mapc
	 (lambda (uid)
	   ;; We now only look at events which were not put there from us
	   (unless (string-match (concat org-caldav-id-string "$") uid)
	     (if (member uid caldav-uids)
		 (org-caldav-debug-print
		  (format "Event UID %s already exists in inbox." uid))
	       (message "Adding new event %s to inbox." uid)
	       (org-caldav-debug-print
		(format "Adding event UID %s to inbox." uid))
	       (with-current-buffer (org-caldav-get-event uid)
		 (apply 'org-caldav-insert-org-entry
			(append (org-caldav-convert-event) (list uid)))))))
	 allevents)))))

(defun org-caldav-generate-ics ()
  "Generate ICS file from `org-caldav-files'.
Returns buffer containing the ICS file."
  (let ((org-combined-agenda-icalendar-file (make-temp-file "org-caldav-")))
    (org-caldav-debug-print (format "Generating ICS file %s."
				    org-combined-agenda-icalendar-file))
    ;; Export events to one single ICS file.
    (apply 'org-export-icalendar t org-caldav-files)
    (find-file-noselect org-combined-agenda-icalendar-file)))

(defun org-caldav-rewrite-uid ()
  "Rewrite UID for event in current buffer."
  (goto-char (point-min))
  (when (null (re-search-forward "^UID:\\s-*\\(.+\\)\\s-*$" nil t))
    (error "No UID could be found for current event."))
  (replace-match "")
  (let ((newuid (concat (md5 (current-buffer)) org-caldav-id-string)))
    (insert (concat "UID:" newuid))
    newuid))

(defun org-caldav-get-preamble ()
  "Snarf preample from current ics file and return it."
  (save-excursion
    (buffer-substring (progn (search-forward "BEGIN:VCALENDAR" nil t)
			     (point-at-bol))
		      (progn (search-forward "BEGIN:VEVENT" nil t)
			     (point-at-bol)))))

(defun org-caldav-current-event-list ()
  "Returns the (maybe cached) event list.
Gets event list from server if no cache is available."
  (unless org-caldav-event-list
    (setq org-caldav-event-list
	  (org-caldav-get-event-list)))
  org-caldav-event-list)

(defun org-caldav-narrow-next-event ()
  "Narrow next event in the current buffer.
If buffer is currently not narrowed, narrow to the first one.
Returns nil if there are no more events."
  (if (not (org-caldav-buffer-narrowed-p))
      (goto-char (point-min))
    (goto-char (point-max))
    (widen))
  (if (null (search-forward "BEGIN:VEVENT" nil t))
      (progn
	;; No more events.
	(widen)	nil)
    (beginning-of-line)
    (narrow-to-region (point)
		      (save-excursion
			(search-forward "END:VEVENT")
			(forward-line 1)
			(point)))
    t))

(defun org-caldav-get-uid-from-event ()
  "Get UID from event in current buffer.
Throw an error if there is no UID."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^UID:\\s-*\\(.+\\)\\s-*$" nil t)
      (error "No UID for event in buffer %s."
	     (buffer-name (current-buffer))))
    (match-string 1)))

(defun org-caldav-get-all-caldav-uids ()
  "Get all CALDAV-UID properties from current org buffer."
  (mapcar 'org-caldav-remove-text-properties
	  (org-property-values "CALDAV-UID")))

(defun org-caldav-debug-print (&rest objects)
  "Print OBJECTS into debug buffer if `org-caldav-debug' is non-nil."
  (when org-caldav-debug
    (with-current-buffer (get-buffer-create org-caldav-debug-buffer)
      (dolist (cur objects)
	(if (stringp cur)
	    (insert cur)
	  (prin1 cur (current-buffer)))
	(insert "\n")))))

(defun org-caldav-buffer-narrowed-p ()
  "Return non-nil if current buffer is narrowed."
  (> (buffer-size) (- (point-max)
		      (point-min))))

(defun org-caldav-remove-text-properties (str)
  "Remove all text properties from string."
  (set-text-properties 0 (length str) nil str)
  str)

(defun org-caldav-filter (events condp)
  "Filter EVENTS according to CONDP."
  (remq nil
	(mapcar (lambda (x) (and (funcall condp x) x)) events)))

(defun org-caldav-insert-org-entry (start-d start-t end-d end-t
					    summary description uid)
  "Insert org block from given data into `org-caldav-inbox'.
START/END-D: Start/End date.  START/END-T: Start/End time.
SUMMARY, DESCRIPTION, UID: obvious.
Dates must be given in a format `org-read-date' can parse."
  (with-current-buffer (find-file-noselect org-caldav-inbox)
    (goto-char (point-max))
    (insert "* " summary "\n  ")
    (when (> (length description) 0)
      (insert description "\n  "))
    (org-caldav-insert-org-time-stamp start-d start-t)
    (if (and end-d
	     (not (equal end-d start-d)))
	(progn
	  (insert "--")
	  (org-caldav-insert-org-time-stamp end-d end-t))
      (when end-t
	;; Same day, different time.
	(backward-char 1)
	(insert "-" end-t)))
    (org-set-property "CALDAV-UID" uid)
    (goto-char (point-max))
    (insert "\n")))

(defun org-caldav-insert-org-time-stamp (date &optional time)
  "Insert org time stamp using DATE and TIME at point.
DATE is given as european date (DD MM YYYY)."
  (let* ((stime (when time (mapcar 'string-to-number
				   (split-string time ":"))))
	 (hours (if time (car stime) 0))
	 (minutes (if time (nth 1 stime) 0))
	 (sdate (mapcar 'string-to-number (split-string date)))
	 (day (car sdate))
	 (month (nth 1 sdate))
	 (year (nth 2 sdate))
	 (internaltime (encode-time 0 minutes hours day month year)))
    (insert
     (concat "<" (if time
		     (format-time-string "%Y-%m-%d %a %H:%M" internaltime)
		   (format-time-string "%Y-%m-%d %a" internaltime))
	     ">"))))

;; The following is taken from icalendar.el, written by Ulf Jasper.

(defun org-caldav-convert-event ()
  "Convert icalendar event in current buffer.
Returns a list '(start-d start-t end-d end-t summary description)'
which can be fed into `org-caldav-insert-org-entry'."
  (goto-char (point-min))
  (let* ((ical-list (icalendar--read-element nil nil))
	 (e (car (icalendar--all-events ical-list)))
	 (zone-map (icalendar--convert-all-timezones ical-list))
	 (dtstart (icalendar--get-event-property e 'DTSTART))
	 (dtstart-zone (icalendar--find-time-zone
			(icalendar--get-event-property-attributes
			 e 'DTSTART)
			zone-map))
	 (dtstart-dec (icalendar--decode-isodatetime dtstart nil
						     dtstart-zone))
	 (start-d (icalendar--datetime-to-diary-date
		   dtstart-dec))
	 (start-t (icalendar--datetime-to-colontime dtstart-dec))
	 (dtend (icalendar--get-event-property e 'DTEND))
	 (dtend-zone (icalendar--find-time-zone
		      (icalendar--get-event-property-attributes
		       e 'DTEND)
		      zone-map))
	 (dtend-dec (icalendar--decode-isodatetime dtend
						   nil dtend-zone))
	 (dtend-1-dec (icalendar--decode-isodatetime dtend -1
						     dtend-zone))
	 end-d
	 end-1-d
	 end-t
	 (summary (icalendar--convert-string-for-import
		   (or (icalendar--get-event-property e 'SUMMARY)
		       "No Title")))
	 (description (icalendar--convert-string-for-import
		       (or (icalendar--get-event-property e 'DESCRIPTION)
			   "")))
	 (rrule (icalendar--get-event-property e 'RRULE))
	 (rdate (icalendar--get-event-property e 'RDATE))
	 (duration (icalendar--get-event-property e 'DURATION)))
    ;; check whether start-time is missing
    (if  (and dtstart
	      (string=
	       (cadr (icalendar--get-event-property-attributes
		      e 'DTSTART))
	       "DATE"))
	(setq start-t nil))
    (when duration
      (let ((dtend-dec-d (icalendar--add-decoded-times
			  dtstart-dec
			  (icalendar--decode-isoduration duration)))
	    (dtend-1-dec-d (icalendar--add-decoded-times
			    dtstart-dec
			    (icalendar--decode-isoduration duration
							   t))))
	(if (and dtend-dec (not (eq dtend-dec dtend-dec-d)))
	    (message "Inconsistent endtime and duration for %s"
		     summary))
	(setq dtend-dec dtend-dec-d)
	(setq dtend-1-dec dtend-1-dec-d)))
    (setq end-d (if dtend-dec
		    (icalendar--datetime-to-diary-date dtend-dec)
		  start-d))
    (setq end-1-d (if dtend-1-dec
		      (icalendar--datetime-to-diary-date dtend-1-dec)
		    start-d))
    (setq end-t (if (and
		     dtend-dec
		     (not (string=
			   (cadr
			    (icalendar--get-event-property-attributes
			     e 'DTEND))
			   "DATE")))
		    (icalendar--datetime-to-colontime dtend-dec)
		  start-t))
    ;; Return result
    (list start-d start-t
	  (if end-t end-d end-1-d)
	  end-t summary description)))


(provide 'org-caldav)

;;; org-caldav.el ends here
