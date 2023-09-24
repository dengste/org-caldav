;;; org-caldav.el --- Sync org files with external calendar through CalDAV   -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2017 Free Software Foundation, Inc.
;; Copyright (C) 2018-2023 David Engster

;; Author: David Engster <deng@randomsample.de>
;; Contributor: Jack Kamm <jackkamm@tatersworld.org>
;; Keywords: calendar, caldav
;; URL: https://github.com/dengste/org-caldav/
;; Package-Requires: ((emacs "26.3") (org "9.1"))
;;
;; Version: 3.0
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

;; See README.

;;; Code:

(require 'url-dav)
(require 'url-http) ;; b/c of Emacs bug
(require 'ox-icalendar)
(require 'org-id)
(require 'icalendar)
(require 'url-util)
(require 'cl-lib)
(require 'button)

(declare-function oauth2-url-retrieve-synchronously "ext:oauth2" (&rest args))
(declare-function oauth2-auth-and-store "ext:oauth2" (&rest args))

(defgroup org-caldav nil
  "Sync org files with external calendar through CalDAV."
  :prefix "org-caldav-"
  :group 'calendar)

(defcustom org-caldav-url "https://my.calendarserver.invalid/caldav"
  "Base URL for CalDAV access.
By default, `org-caldav-calendar-id' will be appended to this to
get an URL for calendar events.  If this default is not correct,
use '%s' to where the calendar id must be placed.

Set this to symbol \\='google to use Google Calendar, using OAuth2
authentication.  In that case, also make sure that
`browse-url-browser-function' is set to a Javascript-capable
browser (like `browse-url-firefox').  Note that you need to have
the OAuth2 library installed, and you will also have to set
`org-caldav-oauth2-client-id' and
`org-caldav-oauth2-client-secret' (see README).

In general, if this variable is a symbol, do OAuth2
authentication with access URIs set in
`org-caldav-oauth2-providers'."
  :type 'string)

(defcustom org-caldav-calendar-id "mycalendar"
  "ID of your calendar."
  :type 'string)

(defcustom org-caldav-uuid-extension ".ics"
  "The file extension to add to uuids in webdav requests.
This is usually .ics, but on some servers (davmail), it is .EML"
  :type 'string)

(defcustom org-caldav-files '("~/org/appointments.org")
  "List of files which should end up in calendar.
The file in `org-caldav-inbox' is implicitly included, so you
don't have to add it here."
  :type '(repeat string))

(defcustom org-caldav-select-tags nil
  "List of tags to filter the synced tasks.
If any such tag is found in a buffer, all items that do not carry
one of these tags will not be exported."
  :type '(repeat string))

(defcustom org-caldav-exclude-tags nil
  "List of tags to exclude from the synced tasks.
All items that carry one of these tags will not be exported."
  :type '(repeat string))

(defcustom org-caldav-inbox "~/org/appointments.org"
  "Where to put new entries obtained from calendar.

This can be simply be a filename to an Org file where all new
entries will be put.  It can also be a list, in which case you
can choose between the following options (which are a subset of
the allowed targets in `org-capture-templates'):

 - (file \"path/to/file\"), or
 - (id \"id of existing org entry\"), or
 - (file+headline \"path/to/file\" \"node headline\"), or
 - (file+olp \"path/to/file\" \"Level 1 headline\" \"Level 2\" ...), or
 - (file+olp+datetree \"path/to/file\" \"Level 1 heading\" ...)

For datetree, use `org-caldav-datetree-treetype' to control the
tree-type; see its Help for more info about the datetree behavior."
  :type 'file)

(defcustom org-caldav-datetree-treetype 'month
  "Tree-type when `org-caldav-inbox' is a datetree.

This can be one of `month', `week', or `day'.

Entries are filed into the datetree according to their start
date. TODO's without a scheduled date instead use the current
date at sync time.

Currently, this only controls where new entries are filed;
existing entries whose dates change won't be refiled
automatically.

Since existing entries aren't refiled automatically yet, the
datetree is more useful for light organization of the org-file,
rather than as a precise reflection of your calendar. Therefore
the default value is at the coarsest level of month-tree."
  :type '(choice
          (const month :tag "Month-tree")
          (const week :tag "Week-Day-tree")
          (const day :tag "Month-Day-tree")))

(defcustom org-caldav-sync-direction 'twoway
  "Which kind of sync should be done between Org and calendar.

Default is `twoway', meaning that changes in Org are synced to the
calendar and changes in the calendar are synced back to
Org. Other options are:

  `org->cal': Only sync Org to calendar
  `cal->org': Only sync calendar to Org"
  :type '(choice
          (const twoway :tag "Two-way sync")
          (const org->cal :tag "Only sync org to calendar")
          (const cal->org :tag "Only sync calendar to Org")))


(defcustom org-caldav-calendars nil
  "A list of plists which define different calendars.
Use this variable to sync with several different remote
calendars.  By setting this, `org-caldav-sync' will run several
times, and you can set the global variables like
`org-caldav-calendar-id' for each run through plist keys.

The available keys are: :url, :calendar-id, :files, :select-tags,
:inbox, :skip-conditions, :sync-inbox, and :sync-direction. They
override the corresponding global org-caldav-* variables. You can
also use any other key, which will then override any org-*
variable.

Example:
\\='((:calendar-id \"work@whatever\" :files (\"~/org/work.org\")
   :inbox \"~/org/fromwork.org\")
  (:calendar-id \"stuff@mystuff\"
   :files (\"~/org/sports.org\" \"~/org/play.org\")
   :inbox \"~/org/fromstuff.org\"))"
  :type '(repeat (plist)))

(defcustom org-caldav-save-directory user-emacs-directory
  "Directory where org-caldav saves its sync state."
  :type 'directory)

(defcustom org-caldav-sync-changes-to-org 'title-and-timestamp
  "What kind of changes should be synced from Calendar to Org.
Can be one of the following symbols:
  title-and-timestamp: Sync title and timestamp (default).
  title-only: Sync only the title.
  timestamp-only: Sync only the timestamp.
  all: Sync everything.

When choosing `all', you should be aware of the fact that the
iCalendar format is pretty limited in what it can store, so you
might loose information in your Org items (take a look at
`org-icalendar-include-body')."
  :type '(choice
          (const title-and-timestamp :tag "Sync title and timestamp")
          (const title-only :tag "Sync only the title")
          (const timestamp-only :tag "Sync only the timestamp")
          (const all :tag "Sync everything")))

(defcustom org-caldav-days-in-past nil
  "Number of days before today to skip in the exported calendar.
This makes it very easy to keep the remote calendar clean.

nil means include all entries (default)
any number set will cut the dates older than N days in the past."
  :type 'integer)

(defcustom org-caldav-delete-org-entries 'ask
  "Whether entries deleted in calendar may be deleted in Org.
Can be one of the following symbols:

ask = Ask for before deletion (default)
never = Never delete Org entries
always = Always delete"
  :type '(choice
          (const ask :tag "Ask before deletion")
          (const never :tag "Never delete Org entries")
          (const always :tag "Always delete")))

(defcustom org-caldav-delete-calendar-entries 'ask
  "Whether entries deleted in Org may be deleted in calendar.
Can be one of the following symbols:

always = Always delete without asking (default)
ask = Ask for before deletion
never = Never delete calendar entries"
  :type '(choice
          (const ask :tag "Ask before deletion")
          (const never :tag "Never delete calendar entries")
          (const always :tag "Always delete without asking")))

(defcustom org-caldav-skip-conditions nil
  "Conditions for skipping entries during icalendar export.
This must be a list of conditions, which are described in the
doc-string of `org-agenda-skip-if'.  Any entry that matches will
not be exported.  Note that the normal `org-agenda-skip-function'
has no effect on the icalendar exporter."
  :type 'list)

(defcustom org-caldav-backup-file
  (expand-file-name "org-caldav-backup.org" user-emacs-directory)
  "Name of the file where org-caldav should backup entries.
Set this to nil if you don't want any backups.

Note that the ID property of the backup entry is renamed to
OLDID, to prevent org-id-find from returning the backup entry in
future syncs."
  :type 'file)

(defcustom org-caldav-show-sync-results 'with-headings
  "Whether to show what was done after syncing.
If this is the symbol `with-headings', the results will also
include headings from Org entries."
  :type '(choice
          (const with-headings :tag "Show what was done after syncing including headings")
          (const nil :tag "Don't show what was done after syncing")))

(defcustom org-caldav-retry-attempts 5
  "Number of times trying to retrieve/put events."
  :type 'integer)

(defcustom org-caldav-calendar-preamble
  "BEGIN:VCALENDAR\nPRODID:-//emacs//org-caldav//EN\nVERSION:2.0\nCALSCALE:GREGORIAN\n"
  "Preamble used for iCalendar events.
You usually should not have to touch this, but it might be
necessary to add timezone information here in case your CalDAV
server does not do that for you, or if you want to use a
different timezone in your Org files."
  :type 'string)

(defcustom org-caldav-sync-todo nil
  "Whether to sync TODO's with the CalDav server. If you enable
this, you should also set `org-icalendar-include-todo' to
`all'.

This feature is relatively new and less well tested; it is
recommended to have backups before enabling it."
  :type 'boolean)

(defcustom org-caldav-todo-priority '((0 nil) (1 "A") (5 "B") (9 "C"))
  "Mapping between iCalendar and Org TODO priority levels.

The iCalendar priority is an integer 1-9, with lower number
having higher priority, and 0 equal to unspecified priority. The
default Org priorities are A-C, but this can be changed with
`org-priority-highest' and `org-priority-lowest'. If you change
the default Org priority, you should also update this
variable (`org-caldav-todo-priority').

The default mapping is: 0 is no priority, 1-4 is #A, 5-8 is #B,
and 9 is #C.

TODO: Store the priority in a property and sync it."
  :type 'list)

(defcustom org-caldav-todo-percent-states '((0 "TODO") (100 "DONE"))
  "Mapping between `org-todo-keywords' & iCal VTODO's percent-complete.

iCalendar's percent-complete is a positive integer between 0 and
100. The default value for `org-caldav-todo-percent-states' maps
these to `org-todo-keywords' as follows: 0-99 is TODO, and 100 is
DONE.

The following example would instead map 0 to TODO, 1 to NEXT,
2-99 to PROG, and 100 to DONE:

  (setq org-caldav-todo-percent-states
        \\='((0 \"TODO\") (1 \"NEXT\") (2 \"PROG\") (100 \"DONE\")))

Note: You should check that the keywords in
`org-caldav-todo-percent-states' are also valid keywords in
`org-todo-keywords'."
  :type 'list)

(defcustom org-caldav-todo-deadline-schedule-warning-days nil
  "Whether to auto-create SCHEDULED timestamp from DEADLINE.

When set to `t', on sync any TODO item with a DEADLINE timestamp
will have a SCHEDULED timestamp added if it doesn't already have
one.

This uses the warning string like DEADLINE: <2017-07-05 Wed -3d>
to a SCHEDULED <2017-07-02 Sun>.  If the warning days (here -3d)
is not given it is taken from `org-deadline-warning-days'.

This might be useful for OpenTasks users, to prevent the app from
showing tasks which have a deadline years in the future."
  :type 'boolean)

(defcustom org-caldav-debug-level 1
  "Level of debug output in `org-caldav-debug-buffer'.
0 or nil: no debug output.  1: Normal debugging.  2: Excessive
debugging (this will also output event content into the
buffer)."
  :type 'integer)

(defcustom org-caldav-debug-buffer "*org-caldav-debug*"
  "Name of the debug buffer."
  :type 'string)

(defcustom org-caldav-resume-aborted 'ask
  "Whether aborted sync attempts should be resumed.
Can be one of the following symbols:

ask = Ask for before resuming (default)
never = Never resume
always = Always resume"
  :type '(choice
          (const ask :tag "Ask before resuming")
          (const never :tag "Never resume")
          (const always :tag "Always resume")))

(defcustom org-caldav-oauth2-providers
  '((google "https://accounts.google.com/o/oauth2/v2/auth"
	    "https://www.googleapis.com/oauth2/v4/token"
	    "https://www.googleapis.com/auth/calendar"
	    "https://apidata.googleusercontent.com/caldav/v2/%s/events"))
  "List of providers that need OAuth2.  Each must be of the form

    IDENTIFIER AUTH-URL TOKEN-URL RESOURCE-URL CALENDAR-URL

where IDENTIFIER is a symbol that can be set in `org-caldav-url'
and '%s' in the CALENDAR-URL denotes where
`org-caldav-calendar-id' must be placed to generate a valid
events URL for a calendar."
  :type 'list)

(defcustom org-caldav-oauth2-client-id nil
  "Client ID for OAuth2 authentication."
  :type 'string)

(defcustom org-caldav-oauth2-client-secret nil
  "Client secret for OAuth2 authentication."
  :type 'string)

(defcustom org-caldav-location-newline-replacement ", "
  "String to replace newlines in the LOCATION field with."
  :type 'string)

(defcustom org-caldav-save-buffers t
  "Whether to save Org buffers modified by sync.

Note this might be needed for some versions of Org (9.5+?), which
have trouble finding IDs in unsaved buffers, causing syncs and
the unit tests to fail otherwise."
  :type 'boolean)

(defcustom org-caldav-description-blank-line-before t
  "Whether DESCRIPTION inserted into org should be preceded by blank line."
  :type 'boolean)

(defcustom org-caldav-description-blank-line-after t
  "Whether DESCRIPTION inserted into org should be followed by blank line."
  :type 'boolean)

;; Internal variables
(defvar org-caldav-oauth2-available
  (condition-case nil (require 'oauth2) (error))
  "Whether oauth2 library is available.")

(defvar org-caldav-previous-calendar nil
  "The plist from org-caldav-calendars, which holds the last
synced calendar. Used to properly resume an interupted attempt.")

(defvar org-caldav-event-list nil
  "The event list database.
This is an alist with elements
  (uid md5 etag sequence status).
It will be saved to disk between sessions.")

(defvar org-caldav-sync-result nil
  "Result from last synchronization.
Contains an alist with entries
  (calendar-id uid status action)

with status = {new,changed,deleted}-in-{org,cal}
and  action = {org->cal, cal->org, error:org->cal, error:cal->org}.")

(defvar org-caldav-empty-calendar nil
  "Flag if we have an empty calendar in the beginning.")

(defvar org-caldav-ics-buffer nil
  "Buffer holding the ICS data.")

(defvar org-caldav-oauth2-tokens nil
  "Tokens for OAuth2 authentication.")

(defvar org-caldav-previous-files nil
  "Files that were synced during previous run.")

(defmacro org-caldav--suppress-obsolete-warning (var body)
  "Macro for compatibility.
To be removed when emacs dependency reaches >=27.1."
  (declare (indent defun))
  (if (fboundp 'with-suppressed-warnings)
      `(with-suppressed-warnings ((obsolete ,var))
         ,body)
    `(with-no-warnings ,body)))

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

(defsubst org-caldav-use-oauth2 ()
  (symbolp org-caldav-url))

(defun org-caldav-filter-events (status)
  "Return list of events with STATUS."
  (delq nil
	(mapcar
	 (lambda (event)
	   (when (eq (car (last event)) status)
	     event))
	 org-caldav-event-list)))

;; Since not being able to access an URL via DAV is the most reported
;; error, let's be very verbose about checking for DAV availability.
(defun org-caldav-check-dav (url)
  "Check if URL accepts DAV requests.
Report an error with further details if that is not the case."
  (let* ((buffer (org-caldav-url-retrieve-synchronously url "OPTIONS")))
    (when (not buffer)
      (error "Retrieving URL %s failed." url))
    (with-current-buffer buffer
      (when (zerop (buffer-size))
	(error "Not data received for URL %s (maybe TLS problem)." url))
      (goto-char (point-min))
      (when (not (re-search-forward "^HTTP[^ ]* \\([0-9]+ .*\\)$"
				    (line-end-position) t))
	(switch-to-buffer buffer)
	(error "No valid HTTP response from URL %s." url))
      (let ((response (match-string 1)))
	(when (not (string-match "2[0-9][0-9].*" response))
	  (switch-to-buffer buffer)
	  (error "Error while checking for OPTIONS at URL %s: %s" url response)))
      (mail-narrow-to-head)
      (let ((davheader (mail-fetch-field "dav")))
	(when (not davheader)
	  (switch-to-buffer buffer)
	  (error "The URL %s does not accept DAV requests" url)))))
  t)

(defun org-caldav-check-oauth2 (provider)
  "Check if we have to do OAuth2 authentication.
If that is the case, also check that everything is installed and
configured correctly, and throw an user error otherwise."
  (when (null (assoc provider org-caldav-oauth2-providers))
    (user-error (concat "No OAuth2 provider found for %s in "
			"`org-caldav-oauth2-providers'")
		(symbol-name provider)))
  (when (not org-caldav-oauth2-available)
    (user-error (concat "Oauth2 library is missing "
			"(install from GNU ELPA)")))
  (when (or (null org-caldav-oauth2-client-id)
	    (null org-caldav-oauth2-client-secret))
    (user-error (concat "You need to set oauth2 client ID and secret "
			"for OAuth2 authentication"))))

(defun org-caldav-retrieve-oauth2-token (provider calendar-id)
  "Do OAuth2 authentication for PROVIDER with CALENDAR-ID."
  (let ((cached-token
	 (assoc
	  (concat (symbol-name provider) "__" calendar-id)
	  org-caldav-oauth2-tokens)))
    (if cached-token
	(cdr cached-token)
      (let* ((ids (assoc provider org-caldav-oauth2-providers))
	     (token (oauth2-auth-and-store (nth 1 ids) (nth 2 ids) (nth 3 ids)
					   org-caldav-oauth2-client-id
					   org-caldav-oauth2-client-secret)))
	(when (null token)
	  (user-error "OAuth2 authentication failed"))
	(setq org-caldav-oauth2-tokens
	      (append org-caldav-oauth2-tokens
		      (list (cons (concat (symbol-name provider) "__" calendar-id)
				  token))))
	token))))

(defun org-caldav-url-retrieve-synchronously (url &optional
					      request-method
					      request-data
					      extra-headers)
  "Retrieve URL with REQUEST-METHOD, REQUEST-DATA and EXTRA-HEADERS.
This will switch to OAuth2 if necessary."
  (if (org-caldav-use-oauth2)
      (oauth2-url-retrieve-synchronously
       (org-caldav-retrieve-oauth2-token org-caldav-url org-caldav-calendar-id)
       url request-method request-data
       extra-headers)
    (let ((url-request-method request-method)
	  (url-request-data request-data)
	  (url-request-extra-headers extra-headers))
      (url-retrieve-synchronously url))))

(defun org-caldav-namespace-bug-workaround (buffer)
  "Workaraound for Emacs bug #23440 on Emacs version <26.
This is needed for the Radicale CalDAV server which uses DAV as
default namespace."
  (when (< emacs-major-version 26)
    (with-current-buffer buffer
      (save-excursion
	(goto-char (point-min))
	(when (re-search-forward "<[^>]* \\(xmlns=\"DAV:\"\\)" nil t)
	  (replace-match "xmlns:DAV=\"DAV:\"" nil nil nil 1)
	  (goto-char (match-beginning 0))
	  (while (re-search-forward "</?" nil t)
	    (insert "DAV:")))))))

(defun org-caldav-url-dav-get-properties (url property)
  "Retrieve PROPERTY from URL.
Output is the same as `url-dav-get-properties'.  This switches to
OAuth2 if necessary."
  (let ((request-data (concat "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n"
			      "<DAV:propfind xmlns:DAV='DAV:'>\n<DAV:prop>"
			      "<DAV:" property "/></DAV:prop></DAV:propfind>\n"))
	(extra '(("Depth" . "1") ("Content-type" . "text/xml"))))
    (let ((resultbuf (org-caldav-url-retrieve-synchronously
                      url "PROPFIND" request-data extra))
          (retr 1))
      (while (and (= 0 (buffer-size resultbuf)) (< retr org-caldav-retry-attempts))
        (org-caldav-debug-print 1 (format "org-caldav-url-dav-get-properties: could not get data from url: %s\n trying again..." url))
        (setq resultbuf (org-caldav-url-retrieve-synchronously
                         url "PROPFIND" request-data extra))
        (setq retr (1+ retr)))
      ;; Check if we got a valid result for PROPFIND
      (with-current-buffer resultbuf
	(goto-char (point-min))
	(when (not (re-search-forward "^HTTP[^ ]* \\([0-9]+ .*\\)$"
				      (line-end-position) t))
	  (switch-to-buffer resultbuf)
	  (error "No valid HTTP response from URL %s." url))
	(let ((response (match-string 1)))
	  (when (not (string-match "2[0-9][0-9].*" response))
	    (switch-to-buffer resultbuf)
	    (error "Error while doing PROPFIND for '%s' at URL %s: %s" property url response))))
      (org-caldav-namespace-bug-workaround resultbuf)
      (url-dav-process-response resultbuf url))))

(defun org-caldav-check-connection ()
  "Check connection by doing a PROPFIND on CalDAV URL.
Also sets `org-caldav-empty-calendar' if calendar is empty."
  (org-caldav-debug-print 1 (format "Check connection for %s."
				    (org-caldav-events-url)))
  (org-caldav-check-dav (org-caldav-events-url))
  (let* ((output (org-caldav-url-dav-get-properties
		  (org-caldav-events-url) "resourcetype"))
	 (status (plist-get (cdar output) 'DAV:status)))
    ;; We accept any 2xx status. Since some CalDAV servers return 404
    ;; for a newly created and not yet used calendar, we accept it as
    ;; well.
    (unless (or (= (/ status 100) 2)
		(= status 404))
      (org-caldav-debug-print 1 "Got error status from PROPFIND: " output)
      (error "Could not query CalDAV URL %s." (org-caldav-events-url)))
    (if (= status 404)
	(progn
	  (org-caldav-debug-print 1 "Got 404 status - assuming calendar is new and empty.")
	  (setq org-caldav-empty-calendar t))
      (when (= (length output) 1)
	;; This is an empty calendar; fetching etags might return 404.
	(org-caldav-debug-print 1 "This is an empty calendar. Setting flag.")
	(setq org-caldav-empty-calendar t)))
    t))

;; This defun is partly taken out of url-dav.el, written by Bill Perry.
(defun org-caldav-get-icsfiles-etags-from-properties (properties)
  "Return all ics files and etags from PROPERTIES."
  (let (prop files)
    (while (setq prop (pop properties))
      (let ((url (car prop))
	    (etag (plist-get (cdr prop) 'DAV:getetag)))
      (if (string-match (concat ".*/\\(.+\\)\\" org-caldav-uuid-extension "/?$") url)
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
    (let ((output (org-caldav-url-dav-get-properties
		   (org-caldav-events-url) "getetag")))
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
If WITH-HEADERS is non-nil, do not delete headers.
If retrieve fails, do `org-caldav-retry-attempts' retries."
  (org-caldav-debug-print 1 (format "Getting event UID %s." uid))
  (let ((counter 0)
	eventbuffer errormessage)
    (while (and (not eventbuffer)
		(< counter org-caldav-retry-attempts))
      (with-current-buffer
	  (org-caldav-url-retrieve-synchronously
	   (concat (org-caldav-events-url) (url-hexify-string uid) org-caldav-uuid-extension))
	(goto-char (point-min))
	(if (looking-at "HTTP.*2[0-9][0-9]")
	    (setq eventbuffer (current-buffer))
	  ;; There was an error retrieving the event
	  (setq errormessage (buffer-substring (point-min) (line-end-position)))
	  (setq counter (1+ counter))
	  (org-caldav-debug-print
	   1 (format "(Try %d) Error when trying to retrieve UID %s: %s"
		     counter uid errormessage)))))
    (unless eventbuffer
      ;; Give up
      (error "Failed to retrieve UID %s after %d tries with error %s"
	     uid org-caldav-retry-attempts errormessage))
    (with-current-buffer eventbuffer
      (unless (search-forward "BEGIN:VCALENDAR" nil t)
	(error "Failed to find calendar entry for UID %s (see buffer %s)"
	       uid (buffer-name eventbuffer)))
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
			      (buffer-string)))
    eventbuffer))

(defun org-caldav-convert-buffer-to-crlf ()
  "Converts local buffer to the dos format using crlf at the end
  of the line.  Some ical validators fail otherwise."
  (save-excursion
    (goto-char (point-min))
    (while (not (= (point) (point-max)))
      (goto-char (- (line-end-position) 1))
      (unless (string= (thing-at-point 'char) "\^M")
        (forward-char)
        (insert "\^M"))
      (forward-line))))

(defun org-caldav-put-event (buffer)
  "Add event in BUFFER to calendar.
The filename will be derived from the UID."
  (let ((event (with-current-buffer buffer (buffer-string))))
    (with-temp-buffer
      (insert org-caldav-calendar-preamble event "END:VCALENDAR\n")
      (goto-char (point-min))
      (let* ((uid (org-caldav-get-uid)))
	(org-caldav-debug-print 1 (format "Putting event UID %s." uid))
	(org-caldav-debug-print 2 (format "Content of event UID %s: " uid)
				(buffer-string))
	(setq org-caldav-empty-calendar nil)
	(org-caldav-save-resource
	 (concat (org-caldav-events-url) uid org-caldav-uuid-extension)
	 (encode-coding-string (buffer-string) 'utf-8))))))

(defun org-caldav-url-dav-delete-file (url)
  "Delete URL.
Will switch to OAuth2 if necessary."
  (org-caldav-url-retrieve-synchronously url "DELETE"))

(defun org-caldav-delete-event (uid)
  "Delete event UID from calendar.
Returns t on success and nil if an error occurs.  The error will
be caught and a message displayed instead."
  (org-caldav-debug-print 1 (format "Deleting event UID %s." uid))
  (condition-case err
      (progn
	(org-caldav-url-dav-delete-file
	 (concat (org-caldav-events-url) uid org-caldav-uuid-extension))
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
  (let* ((url
	  (if (org-caldav-use-oauth2)
	      (nth 4 (assoc org-caldav-url org-caldav-oauth2-providers))
	    org-caldav-url))
	 (eventsurl
	  (if (string-match ".*%s.*" url)
	      (format url org-caldav-calendar-id)
	    (concat url "/" org-caldav-calendar-id "/"))))
    (if (string-match ".*/$" eventsurl)
	eventsurl
      (concat eventsurl "/"))))

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
          ;; FIXME This only detects duplicate events that are new. It
          ;; would also be nice to detect duplicate events that
          ;; changed (#267). But this is complicated to detect b/c
          ;; status 'changed-in-org could be from the previous sync
	  (org-caldav-debug-print 1
	   (format "Org UID %s: Error. Double entry." uid))
	  (push (list org-caldav-calendar-id
		      uid 'new-in-org 'error:double-entry)
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
       ((eq (org-caldav-event-status dbentry) 'ignored)
	(org-caldav-debug-print 1 (format "Cal UID %s: Ignored." (car cur))))
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

(defun org-caldav-var-for-key (key)
  "Return associated global org-caldav variable for key KEY."
  (cl-case key
    (:url 'org-caldav-url)
    (:calendar-id 'org-caldav-calendar-id)
    (:files 'org-caldav-files)
    (:select-tags 'org-caldav-select-tags)
    (:exclude-tags 'org-caldav-exclude-tags)
    (:inbox 'org-caldav-inbox)
    (:skip-conditions 'org-caldav-skip-conditions)
    (:sync-direction 'org-caldav-sync-direction)
    (t (intern
	(concat "org-"
		(substring (symbol-name key) 1))))))

(defsubst org-caldav-sync-do-cal->org ()
  "True if we have to sync from calendar to org."
  (member org-caldav-sync-direction '(twoway cal->org)))

(defsubst org-caldav-sync-do-org->cal ()
  "True if we have to sync from org to calendar."
  (member org-caldav-sync-direction '(twoway org->cal)))

(defun org-caldav-get-org-files-for-sync ()
  "Return list of all org files for syncing.
This adds the inbox if necessary."
  (let ((inbox (org-caldav-inbox-file org-caldav-inbox)))
    (append org-caldav-files
	    (when (and inbox
		       (org-caldav-sync-do-org->cal)
		       (not (member inbox org-caldav-files)))
	      (list inbox)))))

(defun org-caldav-sync-calendar (&optional calendar resume)
  "Sync one calendar, optionally provided through plist CALENDAR.
The format of CALENDAR is described in `org-caldav-calendars'.
If CALENDAR is not provided, the default values will be used.
If RESUME is non-nil, try to resume."
  (setq org-caldav-empty-calendar nil)
  (setq org-caldav-previous-calendar calendar)
  (let (calkeys calvalues)
    ;; Extrace keys and values from 'calendar' for progv binding.
    (dolist (i (number-sequence 0 (1- (length calendar)) 2))
      (setq calkeys (append calkeys (list (nth i calendar)))
	    calvalues (append calvalues (list (nth (1+ i) calendar)))))
    (cl-progv (mapcar 'org-caldav-var-for-key calkeys) calvalues
      (when (org-caldav-sync-do-org->cal)
	(let ((files-for-sync (org-caldav-get-org-files-for-sync)))
	  (dolist (filename files-for-sync)
	    (when (not (file-exists-p filename))
	      (if (yes-or-no-p (format "File %s does not exist, create it?" filename))
		  (write-region "" nil filename)
		(user-error "File %s does not exist" filename))))
	  ;; prevent https://github.com/dengste/org-caldav/issues/230
	  (org-id-update-id-locations files-for-sync)))
      ;; Check if we need to do OAuth2
      (when (org-caldav-use-oauth2)
	;; We need to do oauth2. Check if it is available.
	(org-caldav-check-oauth2 org-caldav-url)
	;; Retrieve token
	(org-caldav-retrieve-oauth2-token org-caldav-url org-caldav-calendar-id))
      (let ((numretry 0)
	    success)
	(while (null success)
	  (condition-case err
	      (progn
		(org-caldav-check-connection)
		(setq success t))
	    (error
	     (if (= numretry (1- org-caldav-retry-attempts))
		 (org-caldav-check-connection)
	       (org-caldav-debug-print
		1 "Got error while checking connection (will try again):" err)
	       (cl-incf numretry))))))
      (unless resume
	(setq org-caldav-event-list nil
	      org-caldav-previous-files nil)
	(org-caldav-load-sync-state)
	;; Check if org files were removed.
	(when org-caldav-previous-files
	  (let ((missing (cl-set-difference org-caldav-previous-files
					    org-caldav-files
					    :test #'string=)))
	    (when (and missing
		       (not (yes-or-no-p
			     (concat "WARNING: Previously synced file(s) are missing: "
				     (mapconcat 'identity missing ",")
				     "%s. Are you sure you want to sync? "))))
	      (user-error "Sync aborted"))))
	;; Remove status in event list
	(dolist (cur org-caldav-event-list)
	  (unless (eq (org-caldav-event-status cur) 'ignored)
	    (org-caldav-event-set-status cur nil)))
	;; Update events for the org->cal direction
	(when (org-caldav-sync-do-org->cal)
	  ;; Export Org to icalendar format
	  (setq org-caldav-ics-buffer (org-caldav-generate-ics))
	  (org-caldav-update-eventdb-from-org org-caldav-ics-buffer))
	;; Update events for the cal->org direction
	(when (org-caldav-sync-do-cal->org)
	  (org-caldav-update-eventdb-from-cal)))
      (when (org-caldav-sync-do-org->cal)
	(org-caldav-update-events-in-cal org-caldav-ics-buffer))
      (when  (org-caldav-sync-do-cal->org)
	(org-caldav-update-events-in-org))
      (org-caldav-save-sync-state)
      (setq org-caldav-event-list nil)
      (when (org-caldav-sync-do-org->cal)
	(with-current-buffer org-caldav-ics-buffer
	  (set-buffer-modified-p nil)
	  (kill-buffer))
	(delete-file (buffer-file-name org-caldav-ics-buffer))))))

;;;###autoload
(defun org-caldav-sync ()
  "Sync Org with calendar."
  (interactive)
  (unless (or (> emacs-major-version 24)
	      (and (= emacs-major-version 24)
		   (> emacs-minor-version 2)))
    (user-error "You have to use at least Emacs 24.3"))
  (org-caldav-debug-print 1 "========== Started sync.")
  (if (and org-caldav-event-list
	   (not (eq org-caldav-resume-aborted 'never))
	   (or (eq org-caldav-resume-aborted 'always)
	       (and (eq org-caldav-resume-aborted 'ask)
	            (y-or-n-p "Last sync seems to have been aborted. \
Should I try to resume? "))))
      (org-caldav-sync-calendar org-caldav-previous-calendar t)
    (setq org-caldav-sync-result nil)
    (if (null org-caldav-calendars)
	(org-caldav-sync-calendar)
      (dolist (calendar org-caldav-calendars)
	(org-caldav-debug-print 1 "Syncing first calendar entry:" calendar)
	(org-caldav-sync-calendar calendar))))
  (when org-caldav-show-sync-results
    (org-caldav-display-sync-results))
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
	  (event-etag (org-caldav-get-event-etag-list))
	  uid)
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
          (while (and (setq uid (org-caldav-get-uid))
                      (not (string-match (car cur) uid))))
          (unless (string-match (car cur) uid)
            (error "Could not find UID %s" (car cur)))
          (org-caldav-narrow-event-under-point)
          (org-caldav-convert-buffer-to-crlf)
          (org-caldav-cleanup-ics-description)
          (org-caldav-maybe-fix-timezone)
          (org-caldav-set-sequence-number cur event-etag)
          (org-caldav-fix-todo-priority)
          (org-caldav-fix-todo-status-percent-state)
          (org-caldav-fix-categories)
          (org-caldav-fix-todo-dtstart)
          (message "Putting event %d of %d Org --> Cal" counter (length events))
          (if (org-caldav-put-event icsbuf)
              (org-caldav-event-set-etag cur 'put)
            (org-caldav-debug-print 1
             (format "Event UID %s: Error while doing Org --> Cal" (car cur)))
            (org-caldav-event-set-status cur 'error)
            (push (list org-caldav-calendar-id (car cur)
                        'error 'error:org->cal)
                  org-caldav-sync-result))))
      ;; Get Etags
      (setq event-etag (org-caldav-get-event-etag-list))
      (dolist (cur events)
	(let ((etag (assoc (car cur) event-etag)))
	  (when (and (not (eq (org-caldav-event-status cur) 'error))
		     etag)
	    (org-caldav-event-set-etag cur (cdr etag))
	    (push (list org-caldav-calendar-id (car cur)
			(org-caldav-event-status cur) 'org->cal)
		  org-caldav-sync-result)))))
    ;; Remove events that were deleted in org
    (unless (eq org-caldav-delete-calendar-entries 'never)
      (let ((events (org-caldav-filter-events 'deleted-in-org))
	    (url-show-status nil)
	    (counter 0))
	(dolist (cur events)
	  (setq counter (1+ counter))
	  (when (or (eq org-caldav-delete-calendar-entries 'always)
		    (y-or-n-p (format "Delete event '%s' from external calendar?"
				       (org-caldav-get-calendar-summary-from-uid
					(car cur)))))
	    (message "Deleting event %d from %d" counter (length events))
	    (org-caldav-delete-event (car cur))
	    (push (list org-caldav-calendar-id (car cur)
			'deleted-in-org 'removed-from-cal)
		  org-caldav-sync-result)
	    (setq org-caldav-event-list
		  (delete cur org-caldav-event-list))))))
    ;; Remove events that could not be put
    (dolist (cur (org-caldav-filter-events 'error))
      (setq org-caldav-event-list
	    (delete cur org-caldav-event-list)))))

(defun org-caldav-set-sequence-number (event event-etag)
  "Set sequence number in ics and in eventdb for EVENT.
EVENT-ETAG is the current list of events and etags on the server.
The ics must be in the current buffer."
  (save-excursion
    (let ((seq (org-caldav-event-sequence event))
	  retrieve)
      (unless (or seq
		  (not (assoc (car event) event-etag)))
	;; We don't have a sequence yet, but event is already in the
	;; calendar, hence we have to get the current number first.
	(setq retrieve (org-caldav-get-event (car event)))
	(when (null retrieve)
	  ;; Retrieving the event failed... so let's just use '1' and
	  ;; hope it works.
	  (org-caldav-debug-print 1 (format "UID %s: Failed to retrieve item from server." (car event)))
	  (org-caldav-debug-print 1 (format "UID %s: Use sequence number 1 and hope for the best." (car event)))
	  (setq seq 0)) ;; incremented below
	(unless seq
	  (with-current-buffer (org-caldav-get-event (car event))
	    (goto-char (point-min))
	    (if (re-search-forward "^SEQUENCE:\\s-*\\([0-9]+\\)" nil t)
		(progn
		  (setq seq (string-to-number (match-string 1)))
		  (org-caldav-debug-print 1 (format "UID %s: Got sequence number %d" (car event) seq)))
	      (org-caldav-debug-print 1 (format "UID %s: Event does not have sequence number, start with 1." (car event)))
	      (setq seq 0))))) ;; incremented below
      (when seq
        (setq seq (1+ seq))
        (goto-char (point-min))
        (if (re-search-forward "^SEQUENCE:" nil t)
            (delete-region (line-beginning-position) (+ 1 (line-end-position)))
	  (goto-char (point-min))
	  (re-search-forward "^SUMMARY:")
	  (forward-line))
        (beginning-of-line)
        (insert "SEQUENCE:" (number-to-string seq) "\n")
        (org-caldav-event-set-sequence event seq)))))

(defun org-caldav-cleanup-ics-description ()
  "Cleanup description for event in current buffer.
This removes an initial timestamp or range if it wasn't removed
by ox-icalendar."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           ;; Can't use org-tsr-regexp because -- is converted to
           ;; unicode emdash –
           (concat "^DESCRIPTION:\\(\\s-*"
                   org-ts-regexp
                   "\\(–"
                   org-ts-regexp
                   "\\)?\\(\\\\n\\\\n\\)?\\)")
            nil t)
      (replace-match "" nil nil nil 1))))

(defun org-caldav-maybe-fix-timezone ()
  "Fix the timezone if it is all uppercase.
This is a bug in older Org versions."
  (unless (null org-icalendar-timezone)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward (upcase org-icalendar-timezone) nil t)
        (replace-match org-icalendar-timezone t)))))

(defun org-caldav-fix-todo-priority ()
  "icalendar exports default priority with ical export.  We want
  a priority of 0 if is not set."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "BEGIN:VTODO" nil t)
      (search-forward "PRIORITY:")
      (unless (eq (thing-at-point 'number) 0)
        ;; NOTE: Deletion up to eol-1 assumes the line ends with ^M
        (delete-region (point) (- (line-end-position) 1))
        (insert (number-to-string
                  (save-excursion
                    (goto-char (point-min))
                    (org-id-goto (org-caldav-get-uid))
                    (org-narrow-to-subtree)
                    (let ((nprio (if (re-search-forward org-priority-regexp nil t)
                                     (let* ((prio (org-entry-get nil "PRIORITY"))
                                            (r 0))
                                       (dolist (pri org-caldav-todo-priority r)
                                         (when (string= (car (cdr pri)) prio)
                                           (setq r (car pri))))
                                       r)
                                   0)))
                      (widen)
                      nprio))))))))

(defun org-caldav-fix-todo-status-percent-state ()
  "icalendar exports only sets the STATUS but not the
PERCENT-COMPLETE.  This works great if you have only TODO and
DONE, but I like to use other states like STARTED or NEXT to
indicate the process.  This fixes the ical values for that.

TODO: save percent-complete also as a property in org"
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "BEGIN:VTODO" nil t)
      (if (search-forward "STATUS:" nil t)
        (delete-region (line-beginning-position) (+ 1 (line-end-position)))
        (progn (search-forward "END:VTODO")
          (goto-char (line-beginning-position))))


      (let* ((state (save-excursion
                      (goto-char (point-min))
                      (org-id-goto (org-caldav-get-uid))
                      (substring-no-properties (org-get-todo-state))))
             (r nil)
             (percent (dolist (p org-caldav-todo-percent-states r)
                        (when (string= state (car (cdr p)))
                          (setq r (car p)))))
             (status (if r
                         (cond ((= percent 0) "NEEDS-ACTION")
                               ((= percent 100) "COMPLETED")
                               (t "IN-PROCESS"))
                       (error "Error setting percent state: '%s' not present in org-caldav-todo-percent-states" state)))
             (completed (save-excursion
                          (goto-char (point-min))
                          (org-id-goto (org-caldav-get-uid))
                          (org-element-property :closed (org-element-at-point)))))
        (insert "PERCENT-COMPLETE:" (number-to-string percent) "\n")
        (insert "STATUS:" status "\n")
        ;; if closed missing but in DONE state:
        (when (and (= percent 100) (not completed))
          (setq completed (save-excursion
                            (goto-char (point-min))
                            (org-id-goto (org-caldav-get-uid))
                            (org-add-planning-info 'closed (org-current-effective-time))
                            (org-element-property :closed (org-element-at-point)))))
        (when completed
          (insert (org-icalendar-convert-timestamp
                    completed "COMPLETED") "\n"))))))


(defun org-caldav-fix-categories ()
  "Nextcloud creates an empty category if this is set without any
  entry.  We fix this by removing the CATEGORIES entry."
  (save-excursion
    (goto-char (point-min))
    (when (and (search-forward "CATEGORIES:" nil t)
            (not (thing-at-point 'word)))
      (delete-region (line-beginning-position) (+ (line-end-position) 1)))))

(defun org-caldav-fix-todo-dtstart ()
  "ox-icalendar includes the actual time as DTSTART into the
vtodo.  For nextcloud this behaviour is undesired, because
dtstart is used for the beginning of the task, which is in the
SCHEDULED of the org entry.  Lets see if the org entry has a
scheduled time and remove dtstart if it doesn't.

If `org-caldav-todo-deadline-schedule-warning-days' is set, this will
also look if there is a deadline."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "BEGIN:VTODO" nil t)
      (when
        (search-forward "DTSTART" nil t)
        (unless
          (save-excursion
            (goto-char (point-min))
            (org-id-goto (org-caldav-get-uid))
            (if (and org-caldav-todo-deadline-schedule-warning-days
                     ;; has deadline warning days set too:
                     (string-match "-\\([0-9]+\\)\\([hdwmy]\\)\\(\\'\\|>\\| \\)"
                                   (or (org-entry-get nil "DEADLINE" nil) "")))
                (or (org-get-scheduled-time nil) (org-get-deadline-time nil))
              (org-get-scheduled-time nil)))
          (delete-region (line-beginning-position) (+ 1 (line-end-position))))))))

(defun org-caldav-inbox-file (inbox)
  "Return file name associated with INBOX.
For format of INBOX, see `org-caldav-inbox'."
  (cond ((stringp inbox)
	 inbox)
	((memq (car inbox) '(file file+headline file+olp file+olp+datetree))
	 (nth 1 inbox))
	((eq (car inbox) 'id)
	 (org-id-find-id-file (nth 1 inbox)))))

(defun org-caldav-inbox-point-and-level (inbox eventdata-alist)
  "Return position and level where to add new entries in INBOX.
For format of INBOX, see `org-caldav-inbox'.  The values are
returned as a cons (POINT . LEVEL)."
  (save-excursion
    (cond ((or (stringp inbox) (eq (car inbox) 'file))
	   (cons (point-max) 1))
	  ((eq (car inbox) 'file+headline)
           ;; FIXME: org-link-search-inhibit-query removed in Org 9.3,
           ;; so byte-compile gives unused lexical variable warning
	   (let ((org-link-search-inhibit-query t))
	     (org-link-search (concat "*" (nth 2 inbox)) nil t)
             (org-caldav--point-level-helper)))
          ((eq (car inbox) 'file+olp)
	   (goto-char (org-find-olp (cdr inbox)))
	   (org-caldav--point-level-helper))
          ((eq (car inbox) 'file+olp+datetree)
           (let ((outline-path (cdr (cdr inbox))))
             (when outline-path
               (goto-char (org-find-olp (cdr inbox))))
             (funcall
              (pcase org-caldav-datetree-treetype
	        (`week #'org-datetree-find-iso-week-create)
	        (`month #'org-caldav--datetree-find-month-create)
	        (`day #'org-datetree-find-date-create))
              (let ((start-d (alist-get 'start-d eventdata-alist)))
                (if start-d (org-caldav--convert-to-calendar start-d)
                  ;; Use current date for a VTODO without DTSTART
                  (calendar-current-date)))
              (when outline-path 'subtree-at-point))
             (org-caldav--point-level-helper)))
	  ((eq (car inbox) 'id)
	   (goto-char (cdr (org-id-find (nth 1 inbox))))
	   (org-caldav--point-level-helper)))))

(defun org-caldav--datetree-find-month-create (d keep-restriction)
  "Helper function for compatibility.
To be removed when emacs dependency reaches >=27.2."
  ;; NOTE: package-lint may give an erroneous warning here. See:
  ;; https://github.com/purcell/package-lint/issues/240
  (if (fboundp 'org-datetree-find-month-create)
      (org-datetree-find-month-create d keep-restriction)
    (error "Need to update to Org 9.4 to use monthtree.")))

(defun org-caldav--point-level-helper ()
  "Helper function for `org-caldav-inbox-point-and-level'.
Go to the end of the current subtree, and return the point and
level to add a new child entry."
  (let ((level (1+ (org-current-level))))
    (org-end-of-subtree t t)
    (cons (point) level)))

(defun org-caldav--todo-percent-to-state (npercent)
  "Converts percentage to keyword in `org-caldav-todo-percent-states'."
  (let (tstate)
    (dolist (to org-caldav-todo-percent-states
                tstate)
      (when (>= npercent (car to))
        (setq tstate (car (cdr to)))))))

(defun org-caldav-update-events-in-org ()
  "Update events in Org files."
  (org-caldav-debug-print 1 "=== Updating events in Org")
  (let ((events (append (org-caldav-filter-events 'new-in-cal)
			(org-caldav-filter-events 'changed-in-cal)))
	(url-show-status nil)
	(counter 0)
	eventdata-alist buf uid timesync is-todo)

    (dolist (cur events)
      (catch 'next
	(setq uid (car cur))
	(setq counter (1+ counter))
	(message "Getting event %d of %d" counter (length events))
	(with-current-buffer (org-caldav-get-event uid)
	  ;; Get sequence number
	  (goto-char (point-min))
          (setq is-todo (when (save-excursion (re-search-forward
                                               "^BEGIN:VTODO$" nil t))
                          t))
	  (when (and is-todo (not org-caldav-sync-todo))
	    (message "Skipping TODO entry.")
	    (org-caldav-event-set-status cur 'ignored)
	    (throw 'next nil))
          (save-excursion
	    (when (re-search-forward "^SEQUENCE:\\s-*\\([0-9]+\\)" nil t)
	      (org-caldav-event-set-sequence
	       cur (string-to-number (match-string 1)))))
	  (setq eventdata-alist (org-caldav-convert-event-or-todo is-todo)))
	(cond
	 ((eq (org-caldav-event-status cur) 'new-in-cal)
	  ;; This is a new event.
	  (condition-case nil
	      (with-current-buffer (find-file-noselect
				    (org-caldav-inbox-file org-caldav-inbox))
		(let ((point-and-level (org-caldav-inbox-point-and-level
                                        org-caldav-inbox eventdata-alist)))
		  (org-caldav-debug-print
		   1 (format "Event UID %s: New in Cal --> Org inbox." uid))
		  (goto-char (car point-and-level))
		  (org-caldav-insert-org-event-or-todo
		   (append eventdata-alist
                           `((uid . ,uid)
                             (level . ,(cdr point-and-level))))))
		(push (list org-caldav-calendar-id uid
			    (org-caldav-event-status cur) 'cal->org)
		      org-caldav-sync-result)
		(setq buf (current-buffer))
                (when org-caldav-save-buffers (save-buffer)))
	    (error
	     ;; inbox file/headline could not be found
	     (org-caldav-event-set-status cur 'error)
	     (push (list org-caldav-calendar-id uid
			 (org-caldav-event-status cur) 'error:inbox-notfound)
		   org-caldav-sync-result)
	     (throw 'next nil))))
	 ((string-match "^orgsexp-" uid)
	  ;; This was generated by a org-sexp, we cannot sync it this way.
	  (org-caldav-debug-print
	   1 (format "Event UID %s: Changed in Cal, but this is a sexp entry \
which can only be synced to calendar. Ignoring." uid))
	  (org-caldav-event-set-status cur 'synced)
	  (push (list org-caldav-calendar-id uid
		      (org-caldav-event-status cur) 'error:changed-orgsexp)
		org-caldav-sync-result)
	  (throw 'next nil))
	 (t
	  ;; This is a changed event.
	  (org-caldav-debug-print
	   1 (format "Event UID %s: Changed in Cal --> Org" uid))
	  (let-alist (append eventdata-alist
                             `((marker . ,(org-id-find (car cur) t))))
	    (when (null .marker)
	      (error "Could not find UID %s." (car cur)))
	    (with-current-buffer (marker-buffer .marker)
	      (goto-char (marker-position .marker))
	      (when org-caldav-backup-file
		(org-caldav-backup-item))
	      ;; See what we should sync.
	      (when (or (eq org-caldav-sync-changes-to-org 'title-only)
			(eq org-caldav-sync-changes-to-org 'title-and-timestamp))
		;; Sync title
		(org-caldav-change-heading .summary)
                (if (not is-todo)
                    ;; Sync location
		    (org-caldav-change-location .location)
                  ;; Sync priority
                  (let* ((nprio (string-to-number (or .priority "0")))
                         (r nil)
                         (vprio (dolist (p org-caldav-todo-priority r)
                                  (when (>= nprio (car p))
                                    (setq r (car (cdr p)))))))
                    (when vprio
                      (org-priority (string-to-char vprio))))
                  ;; Sync todo status
                  (org-todo (org-caldav--todo-percent-to-state
                             (string-to-number .percent-complete)))
                  ;; Sync categories
                  (org-caldav-set-org-tags .categories)))
	      (when (or (eq org-caldav-sync-changes-to-org 'timestamp-only)
			(eq org-caldav-sync-changes-to-org 'title-and-timestamp))
                (if (not is-todo)
                    ;; Sync timestamp; also sets timesync to 'orgsexp
                    ;; if unable to sync due to s-expression
                    (progn
                      (org-narrow-to-subtree)
                      (goto-char (point-min))
		      (setq timesync
                            (if (search-forward "<%%(" nil t)
                                'orgsexp
                              ;; org-caldav-create-time-range can mess
                              ;; with replace-match, so we let-bind tr
                              ;; before calling re-search-forward
                              (let ((tr (org-caldav-create-time-range
                                         .start-d .start-t
                                         .end-d .end-t .end-type)))
                                (when (re-search-forward org-tsr-regexp nil t)
                                  (replace-match tr nil t)))))
                      (widen))
                  ;; Sync scheduled
                  (when .start-d
                    (org--deadline-or-schedule
                     nil 'scheduled (org-caldav-convert-to-org-time
                                     .start-d .start-t)))
                  ;; Sync deadline
                  (when .due-d
                    (org--deadline-or-schedule
                     nil 'deadline (org-caldav-convert-to-org-time
                                    .due-d .due-t)))
                  ;; Sync completion time
                  (when .completed-d (org-add-planning-info
                                      'closed (org-caldav-convert-to-org-time
                                               .completed-d .completed-t)))))
	      (when (eq org-caldav-sync-changes-to-org 'all)
		;; Sync everything, so first remove the old one.
		(let ((level (org-current-level)))
		  (delete-region (org-entry-beginning-position)
				 (org-entry-end-position))
		  (org-caldav-insert-org-event-or-todo
		   (append eventdata-alist `((uid . ,uid)
                                             (level . ,level))))))
	      (setq buf (current-buffer))
	      (push (list org-caldav-calendar-id uid
			  (org-caldav-event-status cur)
			  (if (eq timesync 'orgsexp)
			      'error:changed-orgsexp 'cal->org))
		    org-caldav-sync-result)
              (when org-caldav-save-buffers (save-buffer))))))
	;; Update the event database.
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
		     (y-or-n-p "Delete this entry locally? ")))
	(delete-region (org-entry-beginning-position)
		       (org-entry-end-position))
        (when org-caldav-save-buffers (save-buffer))
	(setq org-caldav-event-list
	      (delete cur org-caldav-event-list))
	(org-caldav-debug-print 1
	 (format "Event UID %s: Deleted from Org" (car cur)))
	(push (list org-caldav-calendar-id (car cur)
		    'deleted-in-cal 'removed-from-org)
	      org-caldav-sync-result)))))

(defun org-caldav--org-show-subtree ()
  "Helper function for compatibility.
To be removed when org dependency reaches >=9.6."
  (if (fboundp 'org-fold-show-subtree)
      (org-fold-show-subtree)
    (org-caldav--suppress-obsolete-warning org-show-subtree
      (org-show-subtree))))

(defun org-caldav-change-heading (newheading)
  "Change heading from Org item under point to NEWHEADING."
  (org-narrow-to-subtree)
  (goto-char (point-min))
  (org-caldav--org-show-subtree)
  (when (and (re-search-forward org-complex-heading-regexp nil t)
	     (match-string 4))
    (let ((start (match-beginning 4))
	  (end (match-end 4)))
      ;; Check if a timestring is in the heading
      (goto-char start)
      (save-excursion
	(when (re-search-forward org-ts-regexp-both end t)
	  ;; Check if timestring is at the beginning or end of heading
	  (if (< (- end (match-end 0))
		 (- (match-beginning 0) start))
	      (setq end (1- (match-beginning 0)))
	    (setq start (1+ (match-end 0))))))
      (delete-region start end)
      (goto-char start)
      (insert newheading)))
  (widen))

(defun org-caldav-change-location (newlocation)
  "Change the LOCATION property from ORG item under point to
NEWLOCATION. If NEWLOCATION is \"\", removes the location property. If
NEWLOCATION contains newlines, replace them with
`org-caldav-location-newline-replacement'."
  (let ((replacement org-caldav-location-newline-replacement))
    (cl-assert (not (string-match-p "\n" replacement)))
    (if (> (length newlocation) 0)
	(org-set-property "LOCATION"
			  (replace-regexp-in-string "\n" replacement newlocation))
      (org-delete-property "LOCATION"))))

(defun org-caldav-backup-item ()
  "Put current item in backup file."
  (let ((item (buffer-substring (org-entry-beginning-position)
				(org-entry-end-position))))
    (with-temp-buffer
      (org-mode)
      (insert item "\n")
      ;; Rename the ID property to OLDID, to prevent org-id-find from
      ;; returning the backup entry in future syncs
      (goto-char (point-min))
      (let* ((entry (org-element-at-point))
             (uid (org-element-property :ID entry)))
        (when uid
          (org-set-property "OLDID" uid)
          (org-delete-property "ID")))
      (write-region (point-min) (point-max) org-caldav-backup-file t))))

(defun org-caldav-skip-function (backend)
  (org-caldav-debug-print 2 "Skipping over excluded entries")
  (when (eq backend 'icalendar)
    (org-map-entries
     (lambda ()
       (let ((pt (save-excursion (apply 'org-agenda-skip-entry-if org-caldav-skip-conditions)))
              (ts (when org-caldav-days-in-past (* (abs org-caldav-days-in-past) -1)))
              (stamp (or (org-entry-get nil "TIMESTAMP" t) (org-entry-get nil "CLOSED" t))))
	 (when (or pt (and stamp ts (> ts (org-time-stamp-to-now stamp))))
           (delete-region (point) (org-end-of-subtree t t))
           (setq org-map-continue-from (point)))))))
  (org-caldav-debug-print 2 "Finished skipping"))

(defun org-caldav-timestamp-has-time-p (timestamp)
  "Checks whether a timestamp has a time.
Returns nil if not and (sec min hour) if it has."
  (let ((ti (org-parse-time-string timestamp)))
    (or (nth 0 ti) (nth 1 ti) (nth 2 ti))))

(defun org-caldav-prepare-scheduled-deadline-timestamps (orgfiles)
  "For nextcloud (or maybe the ical standard?) in vtodo the
scheduled and deadline have all have a time specified or none of
them.  So we find todo items which have deadline and scheduled
specified, but one of them has and the other do not have any
time, and we ask the user to fix that."
  (org-map-entries
    (lambda ()
      (let ((sched (org-entry-get nil "SCHEDULED"))
             (deadl (org-entry-get nil "DEADLINE"))
             kchoice)
        (when (and sched deadl)
          (when (and (org-caldav-timestamp-has-time-p sched)
                  (not (org-caldav-timestamp-has-time-p deadl)))
            (org-id-goto (org-id-get-create))
            (setq kchoice (read-char-choice "Scheduled and Deadline
            set.  For syncing you need to (s) set time on
            DEADLINE, or (d) delete SCHEDULED time."
                        '(?s ?d)))
            (cond ((= kchoice ?s) (org-deadline nil))
              ((= kchoice ?d) (org--deadline-or-schedule nil 'scheduled
                                (replace-regexp-in-string " [0-2][0-9]:[0-5][0-9]" "" sched)))))
          (when (and (not (org-caldav-timestamp-has-time-p sched))
                  (org-caldav-timestamp-has-time-p deadl))
            (org-id-goto (org-entry-get nil "ID"))
            (setq kchoice (read-char-choice "Scheduled and Deadline
            set.  For syncing you need to (s) set time on
            SCHEDULED, or (d) delete DEADLINE time."
                        '(?s ?d)))
            (cond ((= kchoice ?s) (org-schedule nil))
              ((= kchoice ?d) (org--deadline-or-schedule nil 'deadline
                                (replace-regexp-in-string " [0-2][0-9]:[0-5][0-9]" "" sched))))))))
    nil orgfiles))

(defun org-caldav-create-uid (file &optional bell)
  "Set ID property on headlines missing it in FILE.
When optional argument BELL is non-nil, inform the user with
a message if the file was modified. This func is the same as
org-icalendar-create-uid except that it ignores entries that
match org-caldav-skip-conditions."
  (let (modified-flag)
    (org-map-entries
     (lambda ()
       (let ((entry (org-element-at-point)))
         (unless (org-element-property :ID entry)
           (unless (apply 'org-agenda-skip-entry-if org-caldav-skip-conditions)
             (org-id-get-create)
             (setq modified-flag t)
             (forward-line)))))
     nil nil 'comment)
    (when (and bell modified-flag)
      (message "ID properties created in file \"%s\"" file)
      (sit-for 2))))

(defun org-caldav-generate-ics ()
  "Generate ICS file from `org-caldav-files'.
Returns buffer containing the ICS file."
  (let ((icalendar-file
	 (if (featurep 'ox-icalendar)
	     'org-icalendar-combined-agenda-file
	   'org-combined-agenda-icalendar-file))
	(orgfiles (org-caldav-get-org-files-for-sync))
	(org-export-select-tags org-caldav-select-tags)
	(org-icalendar-exclude-tags org-caldav-exclude-tags)
        ;; We create UIDs ourselves and do not rely on ox-icalendar.el
	(org-icalendar-store-UID nil)
	;; Does not work yet
	(org-icalendar-include-bbdb-anniversaries nil)
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
    (dolist (orgfile orgfiles)
      (with-current-buffer (org-get-agenda-file-buffer orgfile)
        (org-caldav-debug-print
         2 (format "Checking %s for new entries & unsaved changes" orgfile))
        (org-caldav-create-uid orgfile t)
        (when (and org-caldav-save-buffers
                   (buffer-modified-p))
          (org-caldav-debug-print 2 (format "Saving %s" orgfile))
          (save-buffer))))
    ;; check scheduled and deadline for having both time or none (vtodo)
    (org-caldav-prepare-scheduled-deadline-timestamps orgfiles)
    (set icalendar-file (make-temp-file "org-caldav-"))
    (org-caldav-debug-print 1 (format "Generating ICS file %s."
				      (symbol-value icalendar-file)))
    ;; compat: use org-export-before-parsing-functions after org >=9.6
    (org-caldav--suppress-obsolete-warning org-export-before-parsing-hook
      (let ((org-export-before-parsing-hook
	     (append org-export-before-parsing-hook
                     (when (or org-caldav-skip-conditions
                               org-caldav-days-in-past)
                       '(org-caldav-skip-function))
                     (when org-caldav-todo-deadline-schedule-warning-days
                       '(org-caldav-scheduled-from-deadline)))))
        ;; Export events to one single ICS file.
        (apply 'org-icalendar--combine-files orgfiles)))
    (find-file-noselect (symbol-value icalendar-file))))

(defun org-caldav-get-uid ()
  "Get UID for event in current buffer."
  (if (re-search-forward "^UID:\\s-*\\(.+\\)\\s-*$" nil t)
      (let ((case-fold-search nil)
            (uid (match-string 1)))
	(while (progn (forward-line)
		      (looking-at " \\(.+\\)\\s-*$"))
	  (setq uid (concat uid (match-string 1))))
	(while (string-match "\\s-+" uid)
	  (setq uid (replace-match "" nil nil uid)))
        (when (string-match "^\\(\\(DL\\|SC\\|TS\\|TODO\\)[0-9]*-\\)" uid)
	  (setq uid (replace-match "" nil nil uid)))
	uid)
    (error "No UID could be found for current event.")))

(defun org-caldav-narrow-next-event ()
  "Narrow next event in the current buffer.
If buffer is currently not narrowed, narrow to the first one.
Returns nil if there are no more events."
  (if (not (org-caldav-buffer-narrowed-p))
      (goto-char (point-min))
    (goto-char (point-max))
    (widen))
  (if (null (re-search-forward "BEGIN:V[EVENT|TODO]" nil t))
      (progn
	;; No more events.
	(widen)	nil)
    (beginning-of-line)
    (narrow-to-region (point)
		      (save-excursion
                        (re-search-forward "END:V[EVENT|TODO]")
			(forward-line 1)
			(point)))
    t))

(defun org-caldav-narrow-event-under-point ()
  "Narrow ics event in the current buffer under point."
  (unless (or (looking-at "BEGIN:VEVENT") (looking-at "BEGIN:VTODO"))
    (when (null (re-search-backward "BEGIN:V[EVENT|TODO]" nil t))
      (error "Cannot find event under point."))
    (beginning-of-line))
  (narrow-to-region (point)
		    (save-excursion
                      (re-search-forward "END:V[EVENT|TODO]")
		      (forward-line 1)
		      (point))))

(defun org-caldav-rewrite-uid-in-event ()
  "Rewrite UID in current buffer.
This will strip prefixes like 'DL' or 'TS' the Org exporter puts
in the UID and also remove whitespaces. Throws an error if there
is no UID to rewrite. Returns the UID."
  (save-excursion
    (goto-char (point-min))
    (let ((uid (org-caldav-get-uid)))
      (when uid
	(goto-char (point-min))
	(re-search-forward "^UID:")
	(let ((pos (point)))
	  (while (progn (forward-line)
			(looking-at " \\(.+\\)\\s-*$")))
	  (delete-region pos (point)))
	(insert uid "\n"))
      uid)))

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

(defun org-caldav-insert-org-event-or-todo (eventdata-alist)
  "Insert org block from given event data at current position.
Elements of EVENTDATA-ALIST are passed on as arguments to
`org-caldav-insert-org-entry' and `org-caldav-insert-org-todo'.
Returns MD5 from entry."
  (let-alist eventdata-alist
    (if (eq .component-type 'todo)
        (org-caldav-insert-org-todo
         .start-d .start-t .due-d .due-t .priority .percent-complete
         .summary .description .completed-d .completed-t .categories
         .uid .level)
      (org-caldav-insert-org-entry
       .start-d .start-t .end-d .end-t .summary
       .description .location .e-type .uid .level))))

(defun org-caldav--insert-description (description)
  (when (> (length description) 0)
    (when org-caldav-description-blank-line-before (newline))
    (let ((beg (point)))
      (insert description)
      (org-indent-region beg (point)))
    (when org-caldav-description-blank-line-after (newline))
    (newline)))

(defun org-caldav-insert-org-entry (start-d start-t end-d end-t
                                            summary description location e-type
                                            &optional uid level)
  "Insert org block from given data at current position.
START/END-D: Start/End date.  START/END-T: Start/End time.
SUMMARY, DESCRIPTION, LOCATION, UID: obvious.
Dates must be given in a format `org-read-date' can parse.

If LOCATION is \"\", no LOCATION: property is written.
If UID is nil, no UID: property is written.
If LEVEL is nil, it defaults to 1.

Returns MD5 from entry."
  (insert (make-string (or level 1) ?*) " " summary "\n")
  (insert (if org-adapt-indentation "  " "")
   (org-caldav-create-time-range start-d start-t end-d end-t e-type) "\n")
  (org-caldav--insert-description description)
  (forward-line -1)
  (when uid
    (org-set-property "ID" (url-unhex-string uid)))
  (org-caldav-change-location location)
  (org-caldav-insert-org-entry--wrapup))

(defun org-caldav-insert-org-todo (start-d start-t due-d due-t
                                    priority percent-complete
                                    summary description
                                    completed-d completed-t
                                    categories
                                    &optional uid level)
  "Insert org block from given data at current position.
START/DUE-D: Start/Due date.  START/DUE-T: Start/Due time.
PRIORITY: 0-9, PERCENT-COMPLETE: 0-100.
See `org-caldav-todo-priority' and
`org-caldav-todo-percent-states' for explanations how this values
are used.
SUMMARY, DESCRIPTION, UID: obvious.
Dates must be given in a format `org-read-date' can parse.

If UID is nil, no UID: property is written.
If LEVEL is nil, it defaults to 1.

Returns MD5 from entry."
  (let* ((nprio (string-to-number (or priority "0")))
          (r nil)
          (vprio (dolist (p org-caldav-todo-priority r)
                   (when (>= nprio (car p))
                     (setq r (car (cdr p))))))
          (prio (if vprio (concat "[#" vprio "] ") "")))
    (insert (make-string (or level 1) ?*) " "
            (org-caldav--todo-percent-to-state
             (string-to-number (or percent-complete "0")))
            " " prio summary "\n"))
  (org-caldav--insert-description description)
  (forward-line -1)
  (when start-d
    (org--deadline-or-schedule
     nil 'scheduled (org-caldav-convert-to-org-time start-d start-t)))
  (when due-d
    (org--deadline-or-schedule
     nil 'deadline (org-caldav-convert-to-org-time due-d due-t)))
  (when completed-d
    (org-add-planning-info 'closed (org-caldav-convert-to-org-time completed-d completed-t)))
  (org-caldav-set-org-tags categories)
  (when uid (org-set-property "ID" (url-unhex-string uid)))
  (org-caldav-insert-org-entry--wrapup))

(defun org-caldav--org-set-tags-to (tags)
  "Helper function for compatibility.
To be removed when org dependency reaches >=9.2."
  (org-caldav--suppress-obsolete-warning org-set-tags-to
    (org-set-tags-to tags)))

(defun org-caldav-insert-org-entry--wrapup ()
  "Helper function to finish inserting an org entry or todo.
Sets the block's tags, and return its MD5."
  (org-back-to-heading)
  (org-caldav--org-set-tags-to org-caldav-select-tags)
  (md5 (buffer-substring-no-properties
	(org-entry-beginning-position)
	(org-entry-end-position))))

(defun org-caldav-scheduled-from-deadline (backend)
  "Create a scheduled entry from deadline."
  (when (eq backend 'icalendar)
    (org-map-entries
     (lambda ()
       (let* ((sched (org-element-property :scheduled (org-element-at-point)))
              (ts (org-element-property :deadline (org-element-at-point)))
              (raw (org-element-property :raw-value ts))
              (wu (org-element-property :warning-unit ts))
              (wv (org-element-property :warning-value ts))
              (dip (when org-caldav-days-in-past (* (abs org-caldav-days-in-past) -1)))
              (stamp (org-entry-get nil "DEADLINE")))
         ;; skip if too old:
         (unless (and dip stamp (> dip (org-time-stamp-to-now stamp)))
         (when (and ts (not sched))
           (org--deadline-or-schedule nil 'scheduled raw)
           (search-forward "SCHEDULED: ")
           (forward-char)
           (if wv
               (progn
                 (cond ((eq wu 'week) (setq wu 'day wv (* wv 7)))
                       ((eq wu 'hour) (setq wu 'minute wv (* wv 60))))
                 (org-timestamp-change (* wv -1) wu))
               (org-timestamp-change (* org-deadline-warning-days -1) 'day))))
         (org-back-to-heading)
         (org-caldav-debug-print 2 (format "scheduled: %s" (org-entry-get nil
                                                                 "SCHEDULED" t))))))))

(defun org-caldav-set-org-tags (tags)
  "Set tags to the headline, where tags is a coma-seperated
  string.  This comes from the ical CATEGORIES line."
  (save-excursion
    (org-back-to-heading)
    (if (> (length tags) 0)
      (let (cleantags)
        (dolist (i (split-string tags ","))
          (setq cleantags (cons
                            (replace-regexp-in-string " " "-" (string-trim i))
                            cleantags)))
        (org-caldav--org-set-tags-to (reverse cleantags)))
      (org-caldav--org-set-tags-to nil))))

(defun org-caldav-create-time-range (start-d start-t end-d end-t e-type)
  "Create an Org timestamp range from START-D/T, END-D/T."
  (with-temp-buffer
    (cond
     ((string= "S" e-type) (insert "SCHEDULED: "))
     ((string= "DL" e-type) (insert "DEADLINE: ")))
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
  (insert
   (concat "<" (org-caldav-convert-to-org-time date time) ">")))

(defun org-caldav-convert-to-org-time (date &optional time)
  "Convert to org time stamp using DATE and TIME.
DATE is given as european date \"DD MM YYYY\"."
  (let* ((stime (when time (mapcar 'string-to-number
                             (split-string time ":"))))
          (hours (if time (car stime) 0))
          (minutes (if time (nth 1 stime) 0))
          (sdate (org-caldav--convert-to-calendar date))
          (internaltime (encode-time 0 minutes hours
                                     (calendar-extract-day sdate)
                                     (calendar-extract-month sdate)
                                     (calendar-extract-year sdate))))
    (if time
        (format-time-string "%Y-%m-%d %a %H:%M" internaltime)
      (format-time-string "%Y-%m-%d %a" internaltime))))

(defun org-caldav--convert-to-calendar (date)
  "Convert DATE to calendar.el-style list (month day year).
DATE is given as european date \"DD MM YYYY\"."
  (let ((sdate (mapcar 'string-to-number (split-string date))))
    (list (nth 1 sdate) (nth 0 sdate) (nth 2 sdate))))

(defun org-caldav-save-sync-state ()
  "Save org-caldav sync database to disk.
See also `org-caldav-save-directory'."
  (with-temp-buffer
    (insert ";; This is the sync state from org-caldav\n;; calendar-id: "
	    org-caldav-calendar-id "\n;; Do not modify this file.\n\n")
    (insert "(setq org-caldav-event-list\n'")
    (let ((print-length nil)
	  (print-level nil))
      (prin1 (delq nil
		   (mapcar (lambda (ev) (unless (eq (org-caldav-event-status ev) 'error) ev))
			   org-caldav-event-list))
	     (current-buffer)))
    (insert ")\n")
    ;; This is just cosmetics.
    (goto-char (point-min))
    (while (re-search-forward ")[^)]" nil t)
      (insert "\n"))
    ;; Save the current value of org-caldav-files
    (insert "(setq org-caldav-previous-files '"
            (let ((print-length nil)
                  (print-level nil))
	            (prin1-to-string org-caldav-files))
      ")\n")
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
  (let ((map (make-sparse-keymap)))
    (define-key map [(tab)] 'forward-button)
    (define-key map [(backtab)] 'backward-button)
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
      (org-caldav-sync-result-print-entries
       (org-caldav-sync-result-filter-errors))
      (insert "\n== Successful syncs: \n\n")
      (org-caldav-sync-result-print-entries
       (org-caldav-sync-result-filter-errors t)))
    (if (fboundp 'pop-to-buffer-same-window)
	(pop-to-buffer-same-window (current-buffer))
      (pop-to-buffer (current-buffer)))
    (setq buffer-read-only t)
    (goto-char (point-min))
    (view-mode-enter)
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
  (if (null entries)
      (insert "None.\n")
    (dolist (entry entries)
      (let ((deleted (or (eq (nth 2 entry) 'deleted-in-org)
			 (eq (nth 2 entry) 'deleted-in-cal))))
	(insert "UID: ")
	(let ((start (point)))
	  (insert (nth 1 entry))
	  (unless deleted
	    (make-button start (point)
             'action (lambda (&rest _ignore)
                       (org-caldav-goto-uid))
             'follow-link t)))
	(when (and (eq org-caldav-show-sync-results 'with-headings)
		   (not deleted))
	  (insert "\n   Title: "
		  (or (org-caldav-get-heading-from-uid (nth 1 entry))
		      "(no title)")))
	(insert "\n   Status: "
		(symbol-name (nth 2 entry))
		"  Action: "
		(symbol-name (nth 3 entry)))
	(when org-caldav-calendars
	  (insert "\n   Calendar: " (car entry)))
	(insert "\n\n")))))

(defun org-caldav-get-heading-from-uid (uid)
  "Get org heading from entry with UID."
  (let ((marker (org-id-find uid t)))
    (if (null marker)
	"(Could not find UID)"
      (with-current-buffer (marker-buffer marker)
	(goto-char (marker-position marker))
	(org-narrow-to-subtree)
	(goto-char (point-min))
	(org-caldav--org-show-subtree)
	(prog1
	    (if (re-search-forward org-complex-heading-regexp nil t)
		(match-string 4)
	      "(Could not find heading)")
	  (widen))))))

(defun org-caldav-goto-uid ()
  "Jump to UID under point."
  (when (button-at (point))
    (beginning-of-line)
    (looking-at "UID: \\(.+\\)$")
    (org-id-goto (match-string 1))))

(defun org-caldav-get-calendar-summary-from-uid (uid)
  "Get summary from UID from calendar."
  (let ((buf (org-caldav-get-event uid))
	(heading ""))
    (when buf
      (with-current-buffer buf
	(goto-char (point-min))
	(when (re-search-forward "^SUMMARY:\\(.*\\)$" nil t)
	  (setq heading (match-string 1)))))
    heading))

(defun org-caldav--datetime-to-colontime (datetime e property &optional default)
  "Extract time part of decoded datetime.
If PROPERTY in event E contains has valuetype \"DATE\" instead of
\"DATE-TIME\", return DEFAULT instead."
  (if (and datetime
             (not (string= 
                   (cadr (icalendar--get-event-property-attributes
                          e property))
                   "DATE")))
      (icalendar--datetime-to-colontime datetime)
    default))

(defun org-caldav--event-date-plist (e property zone-map)
  "Helper function for `org-caldav-convert-event-or-todo'.
Extracts datetime-related attributes from PROPERTY of event E and
puts them in a plist."
  (let* ((dt-prop (icalendar--get-event-property e property))
	 (dt-zone (icalendar--find-time-zone
		   (icalendar--get-event-property-attributes
		    e property)
		   zone-map))
	 (dt-dec (icalendar--decode-isodatetime dt-prop nil dt-zone)))
    (list 'event-property dt-prop
          'zone dt-zone
          'decoded dt-dec
          'date (icalendar--datetime-to-diary-date dt-dec)
          'time (org-caldav--datetime-to-colontime dt-dec e property))))

(defun org-caldav--icalendar--all-todos (icalendar)
  "Return the list of all existing todos in the given ICALENDAR."
  (let ((result '()))
    (mapc (lambda (elt)
            (setq result (append (icalendar--get-children elt 'VTODO)
                           result)))
      (nreverse icalendar))
    result))

;; The following is taken from icalendar.el, written by Ulf Jasper.
;; The LOCATION property is added the extracted list
(defun org-caldav-convert-event-or-todo (is-todo)
  "Convert icalendar event or todo in current buffer.
If IS-TODO, it is a VTODO, else a VEVENT.  Returns an alist of properties
which can be fed into `org-caldav-insert-org-event-or-todo'."
  (let ((decoded (decode-coding-region (point-min) (point-max) 'utf-8 t)))
    (erase-buffer)
    (set-buffer-multibyte t)
    (setq buffer-file-coding-system 'utf-8)
    (insert decoded))
  (goto-char (point-min))
  (let* ((calendar-date-style 'european)
	 (ical-list (icalendar--read-element nil nil))
	 (e (car (if is-todo
                     (org-caldav--icalendar--all-todos ical-list)
                   (icalendar--all-events ical-list))))
	 (zone-map (icalendar--convert-all-timezones ical-list))
         (dtstart-plist (org-caldav--event-date-plist e 'DTSTART zone-map))
         (eventdata-alist
          `((start-d . ,(plist-get dtstart-plist 'date))
            (start-t . ,(plist-get dtstart-plist 'time))
            (dtstart-dec . ,(plist-get dtstart-plist 'decoded))
            (summary . ,(icalendar--convert-string-for-import
		         (or (icalendar--get-event-property e 'SUMMARY)
		             "No Title")))
            (description . ,(icalendar--convert-string-for-import
		             (or (icalendar--get-event-property e 'DESCRIPTION)
			         ""))))))
    (if is-todo
        (org-caldav-convert-event-or-todo--todo e zone-map eventdata-alist)
      (org-caldav-convert-event-or-todo--event e zone-map eventdata-alist))))

(defun org-caldav-convert-event-or-todo--event (e zone-map eventdata-alist)
  "Helper function of `org-caldav-event-or-todo' to handle VEVENT."
  (let* ((start-d (cdr (assq 'start-d eventdata-alist)))
         (start-t (cdr (assq 'start-t eventdata-alist)))
         (dtstart-dec (cdr (assq 'dtstart-dec eventdata-alist)))
         (summary (cdr (assq 'summary eventdata-alist)))
         (dtend-plist (org-caldav--event-date-plist e 'DTEND zone-map))
	 (dtend-dec (plist-get dtend-plist 'decoded))
	 (dtend-1-dec (icalendar--decode-isodatetime
		       (plist-get dtend-plist 'event-property) -1
		       (plist-get dtend-plist 'zone)))
	 e-type
	 (duration (icalendar--get-event-property e 'DURATION)))
    (when (string-match "^\\(?:\\(DL\\|S\\):\s+\\)?\\(.*\\)$" summary)
      (setq e-type (match-string 1 summary))
      (setq summary (match-string 2 summary)))
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
    (let ((end-t (org-caldav--datetime-to-colontime
		  dtend-dec e 'DTEND start-t)))
      ;; Return result
      (append `((component-type . event)
		(end-d
		 . ,(if end-t
			(if dtend-dec
			    (icalendar--datetime-to-diary-date dtend-dec)
			  start-d)
		      (if dtend-1-dec
			  (icalendar--datetime-to-diary-date dtend-1-dec)
			start-d)))
		(end-t . ,end-t)
		(location
		 . ,(icalendar--convert-string-for-import
		     (or (icalendar--get-event-property e 'LOCATION) "")))
		(end-type . ,e-type))
	      eventdata-alist))))

(defun org-caldav-convert-event-or-todo--todo (e zone-map eventdata-alist)
  "Helper function of `org-caldav-event-or-todo' to handle VTODO."
  (let* ((dtdue-plist (org-caldav--event-date-plist e 'DUE zone-map))
	 (dtcomplete-plist (org-caldav--event-date-plist
			    e 'COMPLETED zone-map))
         (percent-complete (icalendar--get-event-property e 'PERCENT-COMPLETE))
         (stat (icalendar--get-event-property e 'STATUS)))
    (unless percent-complete
      (setq percent-complete
        (cond
          ((string= stat "NEEDS-ACTION") "0")
          ((string= stat "IN-PROCESS") "50")
          ((string= stat "COMPLETED") "100")
          (t "0"))))
    (append `((component-type . todo)
	      (due-d . ,(plist-get dtdue-plist 'date))
	      (due-t . ,(plist-get dtdue-plist 'time))
	      (priority . ,(icalendar--get-event-property e 'PRIORITY))
              (percent-complete ., percent-complete)
	      (status . ,stat)
	      (completed-d . ,(plist-get dtcomplete-plist 'date))
	      (completed-t . ,(plist-get dtcomplete-plist 'time))
              (categories . ,(icalendar--convert-string-for-import
                              (or (icalendar--get-event-property e 'CATEGORIES)
                                  ""))))
	    eventdata-alist)))

;; This is adapted from url-dav.el, written by Bill Perry.
;; This does more error checking on the headers and retries
;; in case of an error.
(defun org-caldav-save-resource (url obj)
  "Save string OBJ as URL using WebDAV.
This switches to OAuth2 if necessary."
  (let* ((counter 0)
         errormessage full-response buffer)
    (while (and (not buffer)
		(< counter org-caldav-retry-attempts))
      (with-current-buffer
          (org-caldav-url-retrieve-synchronously
           url "PUT" obj
           '(("Content-type" . "text/calendar; charset=UTF-8")))
        (goto-char (point-min))
        (if (looking-at "HTTP.*2[0-9][0-9]")
            (setq buffer (current-buffer))
          ;; There was an error putting the resource, try again.
          (when (> org-caldav-debug-level 1)
              (setq full-response (buffer-string)))
          (setq errormessage (buffer-substring (point-min) (line-end-position)))
          (setq counter (1+ counter))
	  (org-caldav-debug-print
	   1 (format "(Try %d) Error when trying to put URL %s: %s"
		     counter url errormessage))
	  (kill-buffer))))
    (if buffer
	(kill-buffer buffer)
      (org-caldav-debug-print
       1
       (format "Failed to put URL %s after %d tries with error %s"
               url org-caldav-retry-attempts errormessage))
      (org-caldav-debug-print
       2
        (format "Full error response:\n %s" full-response)))
    (< counter org-caldav-retry-attempts)))

;;;###autoload
(defun org-caldav-import-ics-buffer-to-org ()
  "Add ics content in current buffer to `org-caldav-inbox'."
  (let ((event (org-caldav-convert-event-or-todo nil))
        (file (org-caldav-inbox-file org-caldav-inbox)))
    (with-current-buffer (find-file-noselect file)
      (let* ((point-and-level (org-caldav-inbox-point-and-level
                               org-caldav-inbox event))
             (point (car point-and-level))
             (level (cdr point-and-level)))
        (goto-char point)
        (org-caldav-insert-org-event-or-todo
         (append event `((uid . nil) (level . ,level))))
        (message "%s: Added event: %s"
                 file
                 (buffer-substring
                  point
                  (save-excursion
                    (goto-char point)
                    (line-end-position 2))))))))

;;;###autoload
(defun org-caldav-import-ics-to-org (path)
  "Add ics content in PATH to `org-caldav-inbox'."
  (with-current-buffer (get-buffer-create "*import-ics-to-org*")
    (delete-region (point-min) (point-max))
    (insert-file-contents path)
    (org-caldav-import-ics-buffer-to-org)))

(provide 'org-caldav)

;;; org-caldav.el ends here
