;;; org-caldav.el --- Sync org files with external calendar through CalDAV

;; Copyright (C) 2012-2013 Free Software Foundation, Inc.

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

;; This code is still alpha. Be prepared. Have backups. Take care.
;;
;; Otherwise, see README.

;;; Code:

(require 'url-dav)
(require 'url-http) ;; b/c of Emacs bug
(unless (require 'ox-icalendar nil t)
  (require 'org-icalendar))
(require 'org-id)
(require 'icalendar)
(require 'url-util)

(defvar org-caldav-url "https://www.google.com/calendar/dav"
  "Base URL for CalDAV access.")

(defvar org-caldav-calendar-id "abcde1234@group.calendar.google.com"
  "ID of your calendar.")

(defvar org-caldav-files '("~/org/appointments.org")
  "List of files which should end up in calendar.
The file in `org-caldav-inbox' is implicitly included, so you
don't have to add it here.")

(defvar org-caldav-inbox "~/org/from-calendar.org"
  "Filename for putting new entries obtained from calendar.")

(defvar org-caldav-save-directory user-emacs-directory
  "Directory where org-caldav saves its sync state.")

(defvar org-caldav-sync-changes-to-org 'title-and-timestamp
  "What kind of changes should be synced from Calendar to Org.
Can be one of the following symbols:
  title-and-timestamp: Sync title and timestamp (default).
  title-only: Sync only the title.
  timestamp-only: Sync only the timestamp.
  all: Sync everything.

When choosing 'all', you should be aware of the fact that the
iCalendar format is pretty limited in what it can store, so you
might loose information in your Org items (take a look at
`org-icalendar-include-body').")

(defvar org-caldav-delete-org-entries 'ask
  "Whether entries deleted in calendar may be deleted in Org.
Can be one of the following symbols:

ask = Ask for before deletion (default)
never = Never delete Org entries
always = Always delete")

(defvar org-caldav-backup-file
  (expand-file-name "org-caldav-backup.org" user-emacs-directory)
  "Name of the file where org-caldav should backup entries.
Set this to nil if you don't want any backups.")

(defvar org-caldav-show-sync-results 'with-headings
  "Whether to show what was done after syncing.
If this is the symbol 'with-headings, the results will also
include headings from Org entries.")

(defvar org-caldav-calendar-preamble
  "BEGIN:VCALENDAR\nVERSION:2.0\nCALSCALE:GREGORIAN\n"
  "Preamble used for iCalendar events.
You usually should not have to touch this, but it might be
necessary to add timezone information here in case your CalDAV
server does not do that for you, or if you want to use a
different timezone in your Org files.")

(defvar org-caldav-debug-level 1
  "Level of debug output in `org-caldav-debug-buffer'.
0 or nil: no debug output.  1: Normal debugging.  2: Excessive
debugging (this will also output event content into the
buffer).")

(defvar org-caldav-debug-buffer "*org-caldav-debug*"
  "Name of the debug buffer.")

;; Internal variables

(defvar org-caldav-event-list nil
  "The event list database.
This is an alist with elements
  (uid md5 etag sequence status).
It will be saved to disk between sessions.")

(defvar org-caldav-sync-result nil
  "Result from last synchronization.
Contains an alist with entries
  (uid status action)

with status = {new,changed,deleted}-in-{org,cal}
and  action = {org->cal, cal->org, error:org->cal, error:cal->org}.")

(defvar org-caldav-empty-calendar nil
  "Flag if we have an empty calendar in the beginning.")

(defvar org-caldav-ics-buffer nil
  "Buffer holding the ICS data.")

(defsubst org-caldav-add-event (uid md5 etag sequence status)
  "Add event with UID, MD5, ETAG and STATUS."
  (setq org-caldav-event-list
	(append org-caldav-event-list
		(list (list uid md5 etag sequence status)))))

(defsubst org-caldav-search-event (uid)
  "Return entry with UID from even list."
  (assoc uid org-caldav-event-list))

(defsubst org-caldav-event-md5 (event)
  "Get MD5 from EVENT."
  (nth 1 event))

(defsubst org-caldav-event-etag (event)
  "Get etag from EVENT."
  (nth 2 event))

(defsubst org-caldav-event-sequence (event)
  "Get sequence number from EVENT."
  (nth 3 event))

(defsubst org-caldav-event-status (event)
  "Get status from EVENT."
  (nth 4 event))

(defsubst org-caldav-event-set-status (event status)
  "Set status from EVENT to STATUS."
  (setcar (last event) status))

(defsubst org-caldav-event-set-etag (event etag)
  "Set etag from EVENT to ETAG."
  (setcar (nthcdr 2 event) etag))

(defsubst org-caldav-event-set-md5 (event md5sum)
  "Set md5 from EVENT to MD5SUM."
  (setcar (cdr event) md5sum))

(defsubst org-caldav-event-set-sequence (event seqnum)
  "Set sequence number from EVENT to SEQNUM."
  (setcar (nthcdr 3 event) seqnum))

(defun org-caldav-filter-events (status)
  "Return list of events with STATUS."
  (delq nil
	(mapcar
	 (lambda (event)
	   (when (eq (car (last event)) status)
	     event))
	 org-caldav-event-list)))

(defun org-caldav-check-connection ()
  "Check connection by doing a PROPFIND on CalDAV URL.
Also sets `org-caldav-empty-calendar' if calendar is empty."
  (org-caldav-debug-print 1 (format "Check connection for %s."
				  (org-caldav-events-url)))
  (let ((output (url-dav-get-properties
		 (org-caldav-events-url)
		 '(DAV:resourcetype) 1)))
  (unless (eq (plist-get (cdar output) 'DAV:status) 200)
    (org-caldav-debug-print 1 "Got error status from PROPFIND: " output)
    (error "Could not query CalDAV URL %s." (org-caldav-events-url)))
  (when (= (length output) 1)
    ;; This is an empty calendar; fetching etags might return 404.
    (org-caldav-debug-print 1 "This is an empty calendar. Setting flag.")
    (setq org-caldav-empty-calendar t)))
  t)

;; This defun is partly taken out of url-dav.el, written by Bill Perry.
(defun org-caldav-get-icsfiles-etags-from-properties (properties)
  "Return all ics files and etags from PROPERTIES."
  (let (prop files)
    (while (setq prop (pop properties))
      (let ((url (car prop))
	    (etag (plist-get (cdr prop) 'DAV:getetag)))
      (if (string-match ".*/\\(.+\\)\\.ics/?$" url)
	  (setq url (match-string 1 url))
	(setq url nil))
      (when (string-match "\"\\(.*\\)\"" etag)
	(setq etag (match-string 1 etag)))
      (when (and url etag)
	(push (cons (url-unhex-string url) etag) files))))
    files))

(defun org-caldav-get-event-etag-list ()
  "Return list of events with associated etag from remote calendar.
Return list with elements (uid . etag)."
  (if org-caldav-empty-calendar
      nil
    (let ((output (url-dav-get-properties
		   (org-caldav-events-url)
		   '(DAV:getetag) 1)))
      (cond
       ((> (length output) 1)
	;; Everything looks OK - we got a list of "things".
	;; Get all ics files and etags you can find in there.
	(org-caldav-get-icsfiles-etags-from-properties output))
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
		     (org-caldav-events-url))))))))))

(defun org-caldav-get-event (uid &optional with-headers)
  "Get event with UID from calendar.
Function returns a buffer containing the event, or nil if there's
no such event.
If WITH-HEADERS is non-nil, do not delete headers."
  (org-caldav-debug-print 1 (format "Getting event UID %s." uid))
  (with-current-buffer
      (url-retrieve-synchronously
       (concat (org-caldav-events-url) (url-hexify-string uid) ".ics"))
    (goto-char (point-min))
    (when (search-forward "BEGIN:VCALENDAR" nil t)
      (beginning-of-line)
      (unless with-headers
	(delete-region (point-min) (point)))
      (save-excursion
	(while (re-search-forward "\^M" nil t)
	  (replace-match "")))
      ;; Join lines because of bug in icalendar parsing.
      (save-excursion
	(while (re-search-forward "^ " nil t)
	  (delete-char -2)))
      (org-caldav-debug-print 2 (format "Content of event UID %s: " uid)
			      (buffer-string))
      (current-buffer))))

(defun org-caldav-put-event (buffer)
  "Add event in BUFFER to calendar.
The filename will be derived from the UID."
  (let ((event (with-current-buffer buffer (buffer-string))))
    (with-temp-buffer
      (insert org-caldav-calendar-preamble event "END:VCALENDAR\n")
      (goto-char (point-min))
      (let* ((uid (org-caldav-get-uid))
	     (url (concat (org-caldav-events-url) (url-hexify-string uid) ".ics")))
	(org-caldav-debug-print 1 (format "Putting event UID %s." uid))
	(org-caldav-debug-print 2 (format "Content of event UID %s: " uid)
				(buffer-string))
	(setq org-caldav-empty-calendar nil)
	(org-caldav-save-resource
	 (concat (org-caldav-events-url) uid ".ics")
	 (encode-coding-string (buffer-string) 'utf-8))))))

(defun org-caldav-delete-event (uid)
  "Delete event UID from calendar.
Returns t on success and nil if an error occurs.  The error will
be caught and a message displayed instead."
  (org-caldav-debug-print 1 (format "Deleting event UID %s." uid))
  (condition-case err
      (progn
	(url-dav-delete-file (concat (org-caldav-events-url) uid ".ics"))
	t)
    (error
     (progn
       (message "Could not delete URI %s." uid)
       (org-caldav-debug-print 1 "Got error while removing UID:" err)
       nil))))

(defun org-caldav-delete-everything (prefix)
  "Delete all events from Calendar and removes state file.
Again: This deletes all events in your calendar.  So only do this
if you're really sure.  This has to be called with a prefix, just
so you don't do it by accident."
  (interactive "P")
  (if (not prefix)
      (message "This function has to be called with a prefix.")
    (unless (or org-caldav-empty-calendar
		(not (y-or-n-p "This will delete EVERYTHING in your calendar. \
Are you really sure? ")))
      (let ((events (org-caldav-get-event-etag-list))
	    (counter 0)
	    (url-show-status nil))
	(dolist (cur events)
	  (setq counter (1+ counter))
	  (message "Deleting event %d of %d" counter (length events))
	  (org-caldav-delete-event (car cur)))
	(setq org-caldav-empty-calendar t))
      (when (file-exists-p
	     (org-caldav-sync-state-filename org-caldav-calendar-id))
	(delete-file (org-caldav-sync-state-filename org-caldav-calendar-id)))
      (setq org-caldav-event-list nil)
      (setq org-caldav-sync-result nil)
      (message "Done"))))

(defun org-caldav-events-url ()
  "Return URL for events."
  (if (string-match "google\\.com" org-caldav-url)
      (concat org-caldav-url "/" org-caldav-calendar-id "/events/")
    (concat org-caldav-url "/" org-caldav-calendar-id "/")))

(defun org-caldav-update-eventdb-from-org (buf)
  "With combined ics file in BUF, update the event database."
  (org-caldav-debug-print 1 "=== Updating EventDB from Org")
  (with-current-buffer buf
    (goto-char (point-min))
    (while (org-caldav-narrow-next-event)
      (let* ((uid (org-caldav-rewrite-uid-in-event))
	     (md5 (unless (string-match "^orgsexp-" uid)
		    (org-caldav-generate-md5-for-org-entry uid)))
	     (event (org-caldav-search-event uid)))
	(cond
	 ((null event)
	  ;; Event does not yet exist in DB, so add it.
	  (org-caldav-debug-print 1
	   (format "Org UID %s: New" uid))
	  (org-caldav-add-event uid md5 nil nil 'new-in-org))
	 ((not (string= md5 (org-caldav-event-md5 event)))
	  ;; Event exists but has changed MD5, so mark it as changed.
	  (org-caldav-debug-print 1
	   (format "Org UID %s: Changed" uid))
	  (org-caldav-event-set-md5 event md5)
	  (org-caldav-event-set-status event 'changed-in-org))
	 ((eq (org-caldav-event-status event) 'new-in-org)
	  (org-caldav-debug-print 1
	   (format "Org UID %s: Error. Double entry." uid))
	  (push (list uid 'new-in-org 'error:double-entry)
		org-caldav-sync-result))
	 (t
	  (org-caldav-debug-print 1
	   (format "Org UID %s: Synced" uid))
	  (org-caldav-event-set-status event 'in-org)))))
    ;; Mark events deleted in Org
    (dolist (cur (org-caldav-filter-events nil))
      (org-caldav-debug-print
       1 (format "Cal UID %s: Deleted in Org" (car cur)))
      (org-caldav-event-set-status cur 'deleted-in-org))))

(defun org-caldav-update-eventdb-from-cal ()
  "Update event database from calendar."
  (org-caldav-debug-print 1 "=== Updating EventDB from Cal")
  (let ((events (org-caldav-get-event-etag-list))
	dbentry)
    (dolist (cur events)
      ;; Search entry in database.
      (setq dbentry (org-caldav-search-event (car cur)))
      (cond
       ((not dbentry)
	;; Event is not yet in database, so add it.
	(org-caldav-debug-print 1
	 (format "Cal UID %s: New" (car cur)))
	(org-caldav-add-event (car cur) nil (cdr cur) nil 'new-in-cal))
       ((or (eq (org-caldav-event-status dbentry) 'changed-in-org)
	    (eq (org-caldav-event-status dbentry) 'deleted-in-org))
	(org-caldav-debug-print 1
	 (format "Cal UID %s: Ignoring (Org always wins)." (car cur))))
       ((null (org-caldav-event-etag dbentry))
	(org-caldav-debug-print 1
	 (format "Cal UID %s: No Etag. Mark as change, so putting it again." (car cur)))
	(org-caldav-event-set-status dbentry 'changed-in-org))
       ((not (string= (cdr cur) (org-caldav-event-etag dbentry)))
	;; Event's etag changed.
	(org-caldav-debug-print 1
	 (format "Cal UID %s: Changed" (car cur)))
	(org-caldav-event-set-status dbentry 'changed-in-cal)
	(org-caldav-event-set-etag dbentry (cdr cur)))
       ((null (org-caldav-event-status dbentry))
	;; Event was deleted in Org
	(org-caldav-debug-print 1
	 (format "Cal UID %s: Deleted in Org" (car cur)))
	(org-caldav-event-set-status dbentry 'deleted-in-org))
       ((eq (org-caldav-event-status dbentry) 'in-org)
	(org-caldav-debug-print 1
	 (format "Cal UID %s: Synced" (car cur)))
	(org-caldav-event-set-status dbentry 'synced))
       ((eq (org-caldav-event-status dbentry) 'changed-in-org)
	;; Do nothing
	)
       (t
	(error "Unknown status; this is probably a bug."))))
    ;; Mark events deleted in cal.
    (dolist (cur (org-caldav-filter-events 'in-org))
      (org-caldav-debug-print 1
       (format "Cal UID %s: Deleted in Cal" (car cur)))
      (org-caldav-event-set-status cur 'deleted-in-cal))))

(defun org-caldav-generate-md5-for-org-entry (uid)
  "Find Org entry with UID and calculate its MD5."
  (let ((marker (org-id-find uid t)))
    (when (null marker)
      (error "Could not find UID %s." uid))
    (with-current-buffer (marker-buffer marker)
      (goto-char (marker-position marker))
      (md5 (buffer-substring-no-properties
	    (org-entry-beginning-position)
	    (org-entry-end-position))))))

;;;###autoload
(defun org-caldav-sync ()
  "Sync Org with calendar."
  (interactive)
  (unless (or (bound-and-true-p url-dav-patched-version)
	      (url-dav-supported-p (org-caldav-events-url)))
    (error "You have to either use at least Emacs pretest, \
or the patched `url-dav' package (see Readme)."))
  (org-caldav-debug-print 1 "========== Started sync.")
  (org-caldav-check-connection)
  (unless (and org-caldav-event-list
	       (y-or-n-p "Last sync seems to have been aborted. \
Should I try to resume? "))
    (setq org-caldav-ics-buffer (org-caldav-generate-ics))
    (setq org-caldav-event-list nil)
    (setq org-caldav-sync-result nil)
    (org-caldav-load-sync-state)
    ;; Remove status in event list
    (dolist (cur org-caldav-event-list)
      (org-caldav-event-set-status cur nil))
    (org-caldav-update-eventdb-from-org org-caldav-ics-buffer)
    (org-caldav-update-eventdb-from-cal))
  (org-caldav-update-events-in-cal org-caldav-ics-buffer)
  (org-caldav-update-events-in-org)
  (org-caldav-save-sync-state)
  (setq org-caldav-event-list nil)
  (when org-caldav-show-sync-results
    (org-caldav-display-sync-results))
  (with-current-buffer org-caldav-ics-buffer
    (set-buffer-modified-p nil)
    (kill-buffer))
  (delete-file (buffer-file-name org-caldav-ics-buffer))
  (message "Finished sync."))

(defun org-caldav-update-events-in-cal (icsbuf)
  "Update events in calendar.
ICSBUF is the buffer containing the exported iCalendar file."
  (org-caldav-debug-print 1 "=== Updating events in calendar")
  (with-current-buffer icsbuf
    (widen)
    (goto-char (point-min))
    (let ((events (append (org-caldav-filter-events 'new-in-org)
			  (org-caldav-filter-events 'changed-in-org)))
	  (counter 0)
	  (url-show-status nil)
	  (event-etag (org-caldav-get-event-etag-list)))
      ;; Put the events via CalDAV.
      (dolist (cur events)
	(setq counter (1+ counter))
	(if (eq (org-caldav-event-etag cur) 'put)
	    (org-caldav-debug-print 1
	     (format "Event UID %s: Was already put previously." (car cur)))
	  (org-caldav-debug-print 1
	   (format "Event UID %s: Org --> Cal" (car cur)))
	  (widen)
	  (goto-char (point-min))
	  (search-forward (car cur))
	  (org-caldav-narrow-event-under-point)
	  (org-caldav-cleanup-ics-description)
	  (org-caldav-maybe-fix-timezone)
	  (org-caldav-set-sequence-number cur event-etag)
	  (message "Putting event %d of %d" counter (length events))
	  (if (org-caldav-put-event icsbuf)
	      (org-caldav-event-set-etag cur 'put)
	    (org-caldav-debug-print 1
	     (format "Event UID %s: Error while doing Org --> Cal" (car cur)))
	    (org-caldav-event-set-status cur 'error)
	    (push (list (car cur) 'error 'error:org->cal)
		  org-caldav-sync-result))))
      ;; Get Etags
      (setq event-etag (org-caldav-get-event-etag-list))
      (dolist (cur events)
	(let ((etag (assoc (car cur) event-etag)))
	  (when (and (not (eq (org-caldav-event-status cur) 'error))
		     etag)
	    (org-caldav-event-set-etag cur (cdr etag))
	    (push (list (car cur) (org-caldav-event-status cur) 'org->cal)
		  org-caldav-sync-result)))))
    ;; Remove events that were deleted in org
    (let ((events (org-caldav-filter-events 'deleted-in-org))
	  (url-show-status nil)
	  (counter 0))
      (dolist (cur events)
	(setq counter (1+ counter))
	(message "Deleting event %d from %d" counter (length events))
	(org-caldav-delete-event (car cur))
	(push (list (car cur) 'deleted-in-org 'removed-from-cal)
	      org-caldav-sync-result)
	(setq org-caldav-event-list
	      (delete cur org-caldav-event-list))))
    ;; Remove events that could not be put
    (dolist (cur (org-caldav-filter-events 'error))
      (setq org-caldav-event-list
	    (delete cur org-caldav-event-list)))))

(defun org-caldav-set-sequence-number (event event-etag)
  "Set sequence number in ics and in eventdb for EVENT.
EVENT-ETAG is the current list of events and etags on the server.
The ics must be in the current buffer."
  (save-excursion
    (let ((seq (org-caldav-event-sequence event)))
      (unless (or seq
		  (not (assoc (car event) event-etag)))
	;; We don't have a sequence yet, but event is already in the
	;; calendar, hence we have to get the current number first.
	(with-current-buffer (org-caldav-get-event (car event))
	  (goto-char (point-min))
	  (when (re-search-forward "^SEQUENCE:\\s-*\\([0-9]+\\)" nil t)
	    (org-caldav-event-set-sequence
	     event (string-to-number (match-string 1))))
	  (setq seq (org-caldav-event-sequence event))
	  (org-caldav-debug-print 1 "UID %s: Got sequence number %d" (car event) seq)))
      (when seq
	(setq seq (1+ seq))
	(goto-char (point-min))
	(re-search-forward "^SUMMARY:")
	(forward-line)
	(beginning-of-line)
	(insert "SEQUENCE:" (number-to-string seq) "\n")
	(org-caldav-event-set-sequence event seq)))))

(defun org-caldav-cleanup-ics-description ()
  "Cleanup description for event in current buffer.
This removes timestamps which weren't properly removed by
org-icalendar."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^DESCRIPTION:.*?\\(\\s-*<[^>]+>\\)" nil t)
      (replace-match "" nil nil nil 1))))

(defun org-caldav-maybe-fix-timezone ()
  "Fix the timezone if it is all uppercase.
This is a bug in older Org versions."
  (unless (null org-icalendar-timezone)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward (upcase org-icalendar-timezone) nil t)
	(replace-match org-icalendar-timezone t)))))

(defun org-caldav-update-events-in-org ()
  "Update events in Org files."
  (org-caldav-debug-print 1 "=== Updating events in Org")
  (let ((events (append (org-caldav-filter-events 'new-in-cal)
			(org-caldav-filter-events 'changed-in-cal)))
	(url-show-status nil)
	(counter 0)
	eventdata buf uid timesync)

    (dolist (cur events)
      (setq uid (car cur))
      (setq counter (1+ counter))
      (message "Getting event %d of %d" counter (length events))
      (with-current-buffer (org-caldav-get-event uid)
	;; Get sequence number
	(goto-char (point-min))
	(save-excursion
	  (when (re-search-forward "^SEQUENCE:\\s-*\\([0-9]+\\)" nil t)
	    (org-caldav-event-set-sequence
	     cur (string-to-number (match-string 1)))))
	(setq eventdata (org-caldav-convert-event)))
      (cond
       ((eq (org-caldav-event-status cur) 'new-in-cal)
	;; This is a new event.
	(with-current-buffer (find-file-noselect org-caldav-inbox)
	  (org-caldav-debug-print
	   1 (format "Event UID %s: New in Cal --> Org inbox." uid))
	  (goto-char (point-max))
	  (apply 'org-caldav-insert-org-entry
		 (append eventdata (list uid)))
	  (push (list uid (org-caldav-event-status cur) 'cal->org)
		org-caldav-sync-result)
	  (setq buf (current-buffer))))
       ((string-match "^orgsexp-" uid)
	;; This was generated by a org-sexp, we cannot sync it this way.
	(org-caldav-debug-print
	 1 (format "Event UID %s: Changed in Cal, but this is a sexp entry \
which can only be synced to calendar. Ignoring." uid))
	(org-caldav-event-set-status cur 'synced)
	(push (list uid (org-caldav-event-status cur) 'error:changed-orgsexp)
	      org-caldav-sync-result))
       (t
	;; This is a changed event.
	(org-caldav-debug-print
	 1 (format "Event UID %s: Changed in Cal --> Org" uid))
	(let ((marker (org-id-find (car cur) t)))
	  (when (null marker)
	    (error "Could not find UID %s." (car cur)))
	  (with-current-buffer (marker-buffer marker)
	    (goto-char (marker-position marker))
	    (when org-caldav-backup-file
	      (org-caldav-backup-item))
	    ;; See what we should sync.
	    (when (or (eq org-caldav-sync-changes-to-org 'title-only)
		      (eq org-caldav-sync-changes-to-org 'title-and-timestamp))
	      ;; Sync title
	      (org-caldav-change-heading (nth 4 eventdata)))
	    (when (or (eq org-caldav-sync-changes-to-org 'timestamp-only)
		      (eq org-caldav-sync-changes-to-org 'title-and-timestamp))
	      ;; Sync timestamp
	      (setq timesync
		    (org-caldav-change-timestamp
		     (apply 'org-caldav-create-time-range (butlast eventdata 2)))))
	    (when (eq org-caldav-sync-changes-to-org 'all)
	      ;; Sync everything, so first remove the old one.
	      (delete-region (org-entry-beginning-position)
			     (org-entry-end-position))
	      (apply 'org-caldav-insert-org-entry
		     (append eventdata (list uid))))
	    (setq buf (current-buffer))
	    (if (eq timesync 'orgsexp)
		(push
		 (list uid (org-caldav-event-status cur) 'error:changed-orgsexp)
		 org-caldav-sync-result)
	      (push (list uid (org-caldav-event-status cur) 'cal->org)
		    org-caldav-sync-result))))))
      ;; Update the event database.
      (unless (string-match "^orgsexp-" uid)
	(org-caldav-event-set-status cur 'synced)
	(with-current-buffer buf
	  (org-caldav-event-set-md5
	   cur (md5 (buffer-substring-no-properties
		     (org-entry-beginning-position)
		     (org-entry-end-position))))))))
  ;; (Maybe) delete entries which were deleted in calendar.
  (unless (eq org-caldav-delete-org-entries 'never)
    (dolist (cur (org-caldav-filter-events 'deleted-in-cal))
      (org-id-goto (car cur))
      (when (or (eq org-caldav-delete-org-entries 'always)
		(and (eq org-caldav-delete-org-entries 'ask)
		     (y-or-n-p "Delete this entry? ")))
	(delete-region (org-entry-beginning-position)
		       (org-entry-end-position))
	(setq org-caldav-event-list
	      (delete cur org-caldav-event-list))
	(org-caldav-debug-print 1
	 (format "Event UID %s: Deleted from Org" (car cur)))
	(push (list (car cur) 'deleted-in-cal 'removed-from-org)
	      org-caldav-sync-result)))))

(defun org-caldav-change-heading (newheading)
  "Change heading from Org item under point to NEWHEADING."
  (org-narrow-to-subtree)
  (goto-char (point-min))
  (org-show-subtree)
  (when (and (re-search-forward org-complex-heading-regexp nil t)
	     (match-string 4))
    (let ((start (match-beginning 4))
	  (end (match-end 4)))
      ;; Check if a timestring is in the heading
      (goto-char start)
      (save-excursion
	(when (re-search-forward org-maybe-keyword-time-regexp end t)
	  ;; Check if timestring is at the beginning or end of heading
	  (if (< (- end (match-end 0))
		 (- (match-beginning 0) start))
	      (setq end (1- (match-beginning 0)))
	    (setq start (1+ (match-end 0))))))
      (delete-region start end)
      (goto-char start)
      (insert newheading)))
  (widen))

(defun org-caldav-change-timestamp (newtime)
  "Change timestamp from Org item under point to NEWTIME.
Return symbol 'orgsexp if this entry cannot be changed because it
is on s-expression."
  (org-narrow-to-subtree)
  (goto-char (point-min))
  (if (search-forward "<%%(" nil t)
      'orgsexp
    (when (re-search-forward org-maybe-keyword-time-regexp nil t)
      (replace-match newtime nil t))
    (widen)))

(defun org-caldav-backup-item ()
  "Put current item in backup file."
  (let ((item (buffer-substring (org-entry-beginning-position)
				(org-entry-end-position))))
    (with-current-buffer (find-file-noselect org-caldav-backup-file)
      (goto-char (point-max))
      (insert item "\n")
      (save-buffer))))

(defun org-caldav-generate-ics ()
  "Generate ICS file from `org-caldav-files'.
Returns buffer containing the ICS file."
  (let ((icalendar-file
	 (if (featurep 'ox-icalendar)
	     'org-icalendar-combined-agenda-file
	   'org-combined-agenda-icalendar-file))
	(orgfiles (append org-caldav-files
			  (list org-caldav-inbox)))
	;; We absolutely need UIDs for synchronization.
	(org-icalendar-store-UID t)
	(icalendar-uid-format "orgsexp-%h")
	(org-icalendar-date-time-format
	 (cond
	  ((and org-icalendar-timezone
		(string= org-icalendar-timezone "UTC"))
	   ":%Y%m%dT%H%M%SZ")
	  (org-icalendar-timezone
	   ";TZID=%Z:%Y%m%dT%H%M%S")
	  (t
	   ":%Y%m%dT%H%M%S"))))
    (set icalendar-file (make-temp-file "org-caldav-"))
    (org-caldav-debug-print 1 (format "Generating ICS file %s."
				      (symbol-value icalendar-file)))
    ;; Export events to one single ICS file.
    (if (featurep 'ox-icalendar)
	;; New exporter (Org 8)
	(apply 'org-icalendar--combine-files nil orgfiles)
      (apply 'org-export-icalendar t orgfiles))
    (find-file-noselect (symbol-value icalendar-file))))

(defun org-caldav-get-uid ()
  "Get UID for event in current buffer."
  (goto-char (point-min))
  (if (re-search-forward "^UID:\\s-*\\(.+\\)\\s-*$" nil t)
      (match-string 1)
    (error "No UID could be found for current event.")))

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

(defun org-caldav-narrow-event-under-point ()
  "Narrow ics event in the current buffer under point."
  (unless (looking-at "BEGIN:VEVENT")
    (when (null (search-backward "BEGIN:VEVENT" nil t))
      (error "Cannot find event under point."))
    (beginning-of-line))
  (narrow-to-region (point)
		    (save-excursion
		      (search-forward "END:VEVENT")
		      (forward-line 1)
		      (point))))

(defun org-caldav-rewrite-uid-in-event ()
  "Rewrite UID in current buffer.
This will strip prefixes like 'DL' or 'TS' the Org exporter puts
in the UID and also remove whitespaces. Throws an error if there
is no UID to rewrite. Returns the UID."
  (save-excursion
    (goto-char (point-min))
    (cond
     ((re-search-forward "^UID:\\(orgsexp-[0-9]+\\)" nil t)
      ;; This is a sexp entry, so do nothing.
      (match-string 1))
     ((re-search-forward "^UID:\\(\\s-*\\)\\([A-Z][A-Z][0-9]?-\\)?\\(.+\\)\\s-*$"
			 nil t)
      (when (match-string 1)
	(replace-match "" nil nil nil 1))
      (when (match-string 2)
	(replace-match "" nil nil nil 2))
      (match-string 3))
     (t
      (error "No UID for event in buffer %s."
	     (buffer-name (current-buffer)))))))

(defun org-caldav-debug-print (level &rest objects)
  "Print OBJECTS into debug buffer with debug level LEVEL.
Do nothing if LEVEL is larger than `org-caldav-debug-level'."
  (unless (or (null org-caldav-debug-level)
	      (> level org-caldav-debug-level))
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

(defun org-caldav-insert-org-entry (start-d start-t end-d end-t
					    summary description uid)
  "Insert org block from given data at current position.
START/END-D: Start/End date.  START/END-T: Start/End time.
SUMMARY, DESCRIPTION, UID: obvious.
Dates must be given in a format `org-read-date' can parse.
Returns MD5 from entry."
    (insert "* " summary "\n  ")
    (when (> (length description) 0)
      (insert description "\n  "))
    (insert
     (org-caldav-create-time-range start-d start-t end-d end-t))
    (org-set-property "ID" (url-unhex-string uid))
    (insert "\n")
    (forward-line -1)
    (md5 (buffer-substring-no-properties
	  (org-entry-beginning-position)
	  (org-entry-end-position))))

(defun org-caldav-create-time-range (start-d start-t end-d end-t)
  "Creeate an Org timestamp range from START-D/T, END-D/T."
  (with-temp-buffer
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
    (buffer-string)))

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
     (concat "<"
	     (if time
		 (format-time-string "%Y-%m-%d %a %H:%M" internaltime)
	       (format-time-string "%Y-%m-%d %a" internaltime))
	     ">"))))

(defun org-caldav-save-sync-state ()
  "Save org-caldav sync database to disk.
See also `org-caldav-save-directory'."
  (with-temp-buffer
    (insert ";; This is the sync state from org-caldav\n;; calendar-id: "
	    org-caldav-calendar-id "\n;; Do not modify this file.\n\n")
    (insert "(setq org-caldav-event-list\n'")
    (prin1 org-caldav-event-list (current-buffer))
    (insert ")\n")
    ;; This is just cosmetics.
    (goto-char (point-min))
    (while (re-search-forward ")[^)]" nil t)
      (insert "\n"))
    ;; Save it.
    (write-region (point-min) (point-max)
		  (org-caldav-sync-state-filename org-caldav-calendar-id))))

(defun org-caldav-load-sync-state ()
  "Load org-caldav sync database from disk."
  (let ((filename (org-caldav-sync-state-filename org-caldav-calendar-id)))
    (when (file-exists-p filename)
      (with-temp-buffer
	(insert-file-contents filename)
	(eval-buffer)))))

(defun org-caldav-sync-state-filename (id)
  "Return filename for saving the sync state of calendar with ID."
  (expand-file-name
   (concat "org-caldav-" (substring (md5 id) 1 8) ".el")
   org-caldav-save-directory))

(defvar org-caldav-sync-results-mode-map
  (let ((map (make-keymap)))
    (define-key map [(return)] 'org-caldav-goto-uid)
    (define-key map [(mouse-1)] 'org-caldav-goto-uid)
    map)
  "Keymap for org-caldav result buffer.")

(defun org-caldav-display-sync-results ()
  "Display results of sync in a buffer."
  (with-current-buffer (get-buffer-create "*org caldav sync result*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "CalDAV Sync finished.\n\n")
    (if (null org-caldav-sync-result)
	(insert "Nothing was done.")
      (insert "== Sync errors: \n\n")
      (let ((errevents (org-caldav-sync-result-filter-errors)))
	(if (null errevents)
	    (insert "None.\n")
	  (org-caldav-sync-result-print-entries errevents)))
      (insert "\n== Successful syncs: \n\n")
      (org-caldav-sync-result-print-entries
       (org-caldav-sync-result-filter-errors t)))
    (if (fboundp 'pop-to-buffer-same-window)
	(pop-to-buffer-same-window (current-buffer))
      (pop-to-buffer (current-buffer)))
    (setq buffer-read-only t)
    (goto-char (point-min))
    (use-local-map org-caldav-sync-results-mode-map)))

(defun org-caldav-sync-result-filter-errors (&optional complement)
  "Return items from sync results with errors.
If COMPLEMENT is non-nil, return all item without errors."
  (delq nil
	(mapcar
	 (lambda (x)
	   (if (string-match "^error" (symbol-name (car (last x))))
	       (unless complement x)
	     (when complement x)))
	 org-caldav-sync-result)))

(defun org-caldav-sync-result-print-entries (entries)
  "Helper function to print ENTRIES."
  (dolist (entry entries)
    (let ((deleted (or (eq (nth 1 entry) 'deleted-in-org)
		       (eq (nth 1 entry) 'deleted-in-cal))))
      (insert "UID: ")
      (let ((start (point)))
	(insert (car entry))
	(unless deleted
	  (put-text-property start (point)
			     'face 'link)))
      (when (and (eq org-caldav-show-sync-results 'with-headings)
		 (not deleted))
	(insert "\n   Title: "
		(org-caldav-get-heading-from-uid (car entry))))
      (insert "\n   Status: "
	      (symbol-name (nth 1 entry))
	      "  Action: "
	      (symbol-name (nth 2 entry))
	      "\n\n"))))

(defun org-caldav-get-heading-from-uid (uid)
  "Get org heading from entry with UID."
  (let ((marker (org-id-find uid t)))
    (if (null marker)
	"(Could not find UID)"
      (with-current-buffer (marker-buffer marker)
	(goto-char (marker-position marker))
	(org-narrow-to-subtree)
	(goto-char (point-min))
	(org-show-subtree)
	(prog1
	    (if (re-search-forward org-complex-heading-regexp nil t)
		(match-string 4)
	      "(Could not find heading)")
	  (widen))))))

(defun org-caldav-goto-uid ()
  "Jump to UID unter point."
  (interactive)
  (when (equal (text-properties-at (point))
	       '(face link))
    (beginning-of-line)
    (looking-at "UID: \\(.+\\)$")
    (org-id-goto (match-string 1))))

;; The following is taken from icalendar.el, written by Ulf Jasper.

(defun org-caldav-convert-event ()
  "Convert icalendar event in current buffer.
Returns a list '(start-d start-t end-d end-t summary description)'
which can be fed into `org-caldav-insert-org-entry'."
  (let ((decoded (decode-coding-region (point-min) (point-max) 'utf-8 t)))
    (erase-buffer)
    (set-buffer-multibyte t)
    (setq buffer-file-coding-system 'utf-8)
    (insert decoded))
  (goto-char (point-min))
  (let* ((calendar-date-style 'european)
	 (ical-list (icalendar--read-element nil nil))
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

;; This is adapted from url-dav.el, written by Bill Perry.
;; This does more error checking on the headers.
(defun org-caldav-save-resource (url obj)
  "Save string OBJ as URL using WebDAV."
  (let* ((url-request-extra-headers
	  '(("Content-type" . "text/calendar; charset=UTF-8")))
	 (url-request-method "PUT")
	 (url-request-data obj)
	 (buffer (url-retrieve-synchronously url))
	 result)
    ;; Sanity checking
    (when buffer
      (unwind-protect
	  (with-current-buffer buffer
	    (save-excursion
	      (goto-char (point-min))
	      (let ((answer (buffer-substring (point) (point-at-eol))))
		;; Every 2XX response is OK for us.
		(if (string-match "^HTTP/1.1 2[0-9][0-9]" answer)
		    (setq result t)
		  (org-caldav-debug-print 1
		   (format "Got error: %s" answer)))))
	    (kill-buffer buffer))))
    result))

(provide 'org-caldav)

;;; org-caldav.el ends here
