;; Test suite for org-caldav.el
;; Copyright, authorship, license: see org-caldav.el.

;; Run it from the org-caldav directory like this:
;;   TZ="Europe/Berlin" emacs -Q -L . --eval '(setq org-caldav-url "CALDAV-URL")' -l org-caldav-testsuite.el -f ert
;;
;; On the server, there must already exist two calendars "test1" and "test2".
;; These will completely wiped by running this test!
;;
;; Hint: In case you need a test server, one lightweight option is:
;;    docker run -v /path/to/data:/data tomsquest/docker-radicale
;; Then, you can create the test1 calendar from Thunderbird like so:
;; Thunderbird -> New Calendar -> Network -> Location:
;; http://localhost:5232/test/test1/ (the trailing slash is
;; important), with username "test" and blank password. Then add an
;; event from Thunderbird to make sure the calendar exists.

(require 'ert)
(require 'org)
(require 'org-caldav)
(require 'cl-lib)

(when (org-caldav-use-oauth2)
  (org-caldav-check-oauth2 org-caldav-url)
  (org-caldav-retrieve-oauth2-token org-caldav-url))

(defvar org-caldav-test-calendar-names '("test1" "test2"))

(setq org-caldav-delete-calendar-entries 'always)
(setq org-caldav-backup-file nil)
(setq org-caldav-test-preamble
      "BEGIN:VCALENDAR
VERSION:2.0
CALSCALE:GREGORIAN
X-WR-TIMEZONE:Europe/Berlin
X-WR-CALDESC:
BEGIN:VTIMEZONE
TZID:Europe/Berlin
X-LIC-LOCATION:Europe/Berlin
BEGIN:DAYLIGHT
TZOFFSETFROM:+0100
TZOFFSETTO:+0200
TZNAME:CEST
DTSTART:19700329T020000
RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=-1SU
END:DAYLIGHT
BEGIN:STANDARD
TZOFFSETFROM:+0200
TZOFFSETTO:+0100
TZNAME:CET
DTSTART:19701025T030000
RRULE:FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU
END:STANDARD
END:VTIMEZONE
")

;; First test event in calendar
(setq org-caldav-test-ics1
"BEGIN:VEVENT
DTSTART;VALUE=DATE:20121220
DTEND;VALUE=DATE:20121221
DTSTAMP:20121218T212132Z
UID:orgcaldavtest@cal1
CREATED:20121216T205929Z
DESCRIPTION:A first test
LAST-MODIFIED:20121218T212132Z
LOCATION:
SUMMARY:Test appointment Number 1
END:VEVENT
")

;; How it should end up in Org
(setq org-caldav-test-ics1-org
      "\\* Test appointment Number 1
\\s-*:PROPERTIES:
\\s-*:ID:\\s-*orgcaldavtest@cal1
\\s-*:END:
\\s-*<2012-12-20 Thu>
\\s-*A first test")

;; Dto., second one
(setq org-caldav-test-ics2
      "BEGIN:VEVENT
DTSTART;TZID=Europe/Berlin:20121205T190000
DTEND;TZID=Europe/Berlin:20121205T200000
DTSTAMP:20121219T213352Z
UID:orgcaldavtest-cal2
CREATED:20121219T213352Z
DESCRIPTION:A second test
LAST-MODIFIED:20121219T213352Z
LOCATION:
SUMMARY:Test appointment Number 2
END:VEVENT
")

(setq org-caldav-test-ics2-org
      "\\* Test appointment Number 2
\\s-*:PROPERTIES:
\\s-*:ID:\\s-*orgcaldavtest-cal2
\\s-*:END:
\\s-*<2012-12-05 Wed 19:00-20:00>
\\s-*A second test")

;; First test task in calendar
(setq org-caldav-test-ics3
      "BEGIN:VTODO
UID:orgcaldavtest@cal3
DTSTAMP:20220828T161432Z
DTSTART;VALUE=DATE:20121223
SUMMARY:A test task from iCal
DESCRIPTION:ical test task 1
PRIORITY:0
STATUS:NEEDS-ACTION
END:VTODO
")

(setq org-caldav-test-ics3-org
      "\\* TODO A test task from iCal
\\s-*SCHEDULED: <2012-12-23 Sun>
\\s-*:PROPERTIES:
\\s-*:ID:       orgcaldavtest@cal3
\\s-*:END:
\\s-*ical test task 1")

;; Second test task in calendar
(setq org-caldav-test-ics4
      "BEGIN:VTODO
UID:orgcaldavtest-cal4
DTSTAMP:20220828T161432Z
DTSTART;VALUE=DATE:20121219
DUE;VALUE=DATE:20121223
SUMMARY:Another test task from iCal
DESCRIPTION:ical test task 2
PRIORITY:5
STATUS:NEEDS-ACTION
END:VTODO
")

(setq org-caldav-test-ics4-org
      "\\* TODO \\[#B\\] Another test task from iCal
\\s-*DEADLINE: <2012-12-23 Sun> SCHEDULED: <2012-12-19 Wed>
\\s-*:PROPERTIES:
\\s-*:ID:       orgcaldavtest-cal4
\\s-*:END:
\\s-*ical test task 2")

;; First test entry in Org which should end up in calendar
(setq org-caldav-test-org1
      "* This is a test
:PROPERTIES:
:ID:       orgcaldavtest@org1
:END:
<2012-12-23 Sun 20:00-21:00>
Foo Bar Baz
")

;; Dto., second one
(setq org-caldav-test-org2
      "* This is another test
:PROPERTIES:
:ID:       orgcaldavtest-org2
:END:
<2012-12-19 Wed 19:00-21:00>
Baz Bar Foo
")

(setq org-caldav-test-org3
      "* This is a test with a tag :sometag:
:PROPERTIES:
:ID:       orgcaldavtest-org3
:END:
<2012-12-20 Thu 19:00-21:00>
moose
")

;; First test task in Org which should end up in calendar
(setq org-caldav-test-org4
      "* TODO A test task from Org
SCHEDULED: <2012-12-23 Sun>
:PROPERTIES:
:ID:       orgcaldavtest@org4
:END:
Org task 1
")

;; Dto., second one
(setq org-caldav-test-org5
      "* TODO [#B] Another test task from Org
DEADLINE: <2012-12-23 Sun> SCHEDULED: <2012-12-19 Wed>
:PROPERTIES:
:ID:       orgcaldavtest-org5
:END:
Org task 2
")

;; All events after sync.
(setq org-caldav-test-allevents
      '("orgcaldavtest@org1" "orgcaldavtest-org2" "orgcaldavtest@cal1" "orgcaldavtest-cal2"))

(setq org-caldav-test-alltodos
      '("orgcaldavtest@org4" "orgcaldavtest-org5" "orgcaldavtest@cal3" "orgcaldavtest-cal4"))


(setq org-caldav-test-sync-result
      '(("test1" "orgcaldavtest@cal1" new-in-cal cal->org)
	("test1" "orgcaldavtest-cal2" new-in-cal cal->org)))

;; Test files.
(defun org-caldav-test-calendar-empty-p ()
  "Check if calendar is empty."
  (let ((output (org-caldav-url-dav-get-properties
		 (org-caldav-events-url) "resourcetype")))
    (unless (eq (plist-get (cdar output) 'DAV:status) 200)
      (error "Could not query CalDAV URL %s: %s." (org-caldav-events-url) (prin1-to-string output)))
    (= (length output) 1)))

(defun org-caldav-test-set-up ()
  "Make a clean slate."
  (message "SET UP")
  (unless (org-caldav-test-calendar-empty-p)
    (dolist (cur (org-caldav-get-event-etag-list))
      (message "Deleting %s" (car cur))
      (org-caldav-delete-event (car cur))))
  (should (org-caldav-test-calendar-empty-p)))

(defun org-caldav-test-put-events ()
  "Put initial events to calendar."
  (message "PUT")
  (let ((org-caldav-calendar-preamble org-caldav-test-preamble)
	events)
    (with-temp-buffer
      (insert org-caldav-test-ics1)
      (should (org-caldav-put-event (current-buffer)))
      (erase-buffer)
      (insert org-caldav-test-ics2)
      (should (org-caldav-put-event (current-buffer))))
    (should
     (setq events
	   (org-caldav-get-event-etag-list)))
    (should (assoc "orgcaldavtest@cal1" events))
    (should (assoc "orgcaldavtest-cal2" events))))

(defun org-caldav-test-setup-temp-files ()
  (let ((tmpdir (make-temp-file "org-caldav-test-" t)))
    (message "Using tempdir %s" tmpdir)
    (setq org-caldav-save-directory (expand-file-name "org-caldav-savedir" tmpdir)
	  org-caldav-test-orgfile (expand-file-name "test.org" tmpdir)
	  org-caldav-test-second-orgfile (expand-file-name "test-second.org" tmpdir)
	  org-caldav-test-inbox (expand-file-name "inbox.org" tmpdir)
	  org-id-locations-file (expand-file-name "org-id-locations" tmpdir)
	  org-id-locations nil
	  org-id-files nil
	  org-caldav-test-current-tempdir tmpdir))
  (make-directory org-caldav-save-directory)
  (with-current-buffer (find-file-noselect org-caldav-test-inbox)
    (save-buffer)))

(defun org-caldav-test-cleanup ()
  (dolist (name '("test.org" "test-second.org" "inbox.org" "org-id-locations"))
    (let ((buf (get-buffer name)))
      (when buf
	(with-current-buffer buf
	  (set-buffer-modified-p nil)
	  (kill-buffer)))))
  (setq org-id-locations nil)
  (setq org-caldav-event-list nil)
  (when org-caldav-test-current-tempdir
    (delete-directory org-caldav-test-current-tempdir t)
    (setq org-caldav-test-current-tempdir nil)))

;; This is one, big, big test, since pretty much everything depends on
;; the current calendar/org state and I cannot easily split it.
(ert-deftest org-caldav-01-sync-test ()
  (message "Setting up temporary files")
  (org-caldav-test-setup-temp-files)
  (setq org-caldav-calendar-id (car org-caldav-test-calendar-names))
  ;; Set up orgfile.
  (with-current-buffer (find-file-noselect org-caldav-test-orgfile)
    (insert org-caldav-test-org1 "\n")
    (insert org-caldav-test-org2 "\n")
    (save-buffer))
  ;; Set up data for org-caldav.
  (setq org-caldav-files (list org-caldav-test-orgfile))
  (setq org-caldav-inbox org-caldav-test-inbox)
  (setq org-caldav-debug-level 2)
  (message "Cleaning up upstream calendars")
  (org-caldav-test-set-up)
  (message "Putting events")
  (org-caldav-test-put-events)
  (message "1st SYNC")
  ;; Do the sync.
  (org-caldav-sync)
  ;; Check result.
  (should (member `(,org-caldav-calendar-id "orgcaldavtest@cal1" new-in-cal cal->org)
		  org-caldav-sync-result))
  (should (member `(,org-caldav-calendar-id "orgcaldavtest-cal2" new-in-cal cal->org)
		  org-caldav-sync-result))
  (should (member `(,org-caldav-calendar-id "orgcaldavtest@org1" new-in-org org->cal)
		  org-caldav-sync-result))
  (should (member `(,org-caldav-calendar-id "orgcaldavtest-org2" new-in-org org->cal)
		  org-caldav-sync-result))
  ;; State file should exist now.
  (should (file-exists-p
	   (org-caldav-sync-state-filename org-caldav-calendar-id)))
  (let ((calevents (org-caldav-get-event-etag-list)))
    (should (= (length calevents) (length org-caldav-test-allevents)))
    ;; Org events should be in cal.
    (dolist (cur org-caldav-test-allevents)
      (should (assoc cur calevents))))
  ;; Cal events should be in Org.
  (with-current-buffer (find-file-noselect org-caldav-test-inbox)
    (goto-char (point-min))
    (should (re-search-forward org-caldav-test-ics1-org nil t))
    (goto-char (point-min))
    (should (re-search-forward org-caldav-test-ics2-org nil t)))

  (message "2nd SYNC")
  ;; Sync again.
  (org-caldav-sync)
  ;; Nothing should have happened.
  (should-not org-caldav-sync-result)
  (message "Changing org events")
  ;; Now change events in Org
  (with-current-buffer (find-buffer-visiting org-caldav-test-orgfile)
    (goto-char (point-min))
    (search-forward "This is another test")
    (replace-match "This is a changed test heading")
    (search-forward "<2012-12-19 Wed 19:00-21:00>")
    (replace-match "<2012-12-19 Thu 20:00-22:00>"))
  (with-current-buffer (find-buffer-visiting org-caldav-test-inbox)
    (goto-char (point-min))
    (search-forward "Test appointment Number 2")
    (replace-match "Appointment number 2 was changed!")
    (search-forward "<2012-12-05 Wed 19:00-20:00>")
    (replace-match "<2012-12-04 Tue 18:00-19:00>"))

  ;; And sync...
  (message "3rd SYNC")
  (org-caldav-sync)
  (should (equal `((,org-caldav-calendar-id "orgcaldavtest-cal2" changed-in-org org->cal)
		   (,org-caldav-calendar-id "orgcaldavtest-org2" changed-in-org org->cal))
		 org-caldav-sync-result))

  ;; Check if those events correctly end up in calendar.
  (with-current-buffer (org-caldav-get-event "orgcaldavtest-cal2")
    (goto-char (point-min))
    (save-excursion
      (should (search-forward "SUMMARY:Appointment number 2 was changed!")))
    (save-excursion
      (should (re-search-forward "DTSTART.*:20121204T\\(170000Z\\|180000\\)" nil t)))
    (save-excursion
      (should (re-search-forward "DTEND.*:20121204T\\(180000Z\\|190000\\)" nil t))))

  (with-current-buffer (org-caldav-get-event "orgcaldavtest-org2")
    (goto-char (point-min))
    (save-excursion
      (should (search-forward "SUMMARY:This is a changed test heading")))
    (save-excursion
      (should (re-search-forward "DTSTART.*:20121219T\\(190000Z\\|200000\\)" nil t)))
    (save-excursion
      (should (re-search-forward "DTEND.*:20121219T\\(210000Z\\|220000\\)" nil t))))

  ;; Now change events in Cal
  (message "Changing events in calendar")
  (with-current-buffer (org-caldav-get-event "orgcaldavtest@cal1")
    (goto-char (point-min))
    (save-excursion
      (search-forward "SUMMARY:Test appointment Number 1")
      (replace-match "SUMMARY:Changed test appointment Number 1"))
    (save-excursion
      (re-search-forward "DTSTART\\(;.*\\)?:\\(20121220\\)")
      (replace-match "20121212" nil nil nil 2))
    (save-excursion
      (re-search-forward "DTEND\\(;.*\\)?:\\(20121221\\)")
      (replace-match "20121213" nil nil nil 2))
    (save-excursion
      (when (re-search-forward "SEQUENCE:\\s-*\\([0-9]+\\)" nil t)
	(replace-match (number-to-string
			(1+ (string-to-number (match-string 1))))
		       nil t nil 1)))
    (message "PUTting first changed event")
    (should (org-caldav-save-resource
	     (concat (org-caldav-events-url) (url-hexify-string "orgcaldavtest@cal1.ics"))
	     (encode-coding-string (buffer-string) 'utf-8))))

  (with-current-buffer (org-caldav-get-event "orgcaldavtest@org1")
    (goto-char (point-min))
    (save-excursion
      (search-forward "SUMMARY:This is a test")
      (replace-match "SUMMARY:This is a changed test"))
    (save-excursion
      (if (re-search-forward "DTSTART\\(;.*\\)?:\\(20121223T190000Z\\)" nil t)
	  (replace-match "20121213T180000Z" nil nil nil 2)
	(re-search-forward "DTSTART\\(;.*\\)?:\\(20121223T200000\\)")
	(replace-match "20121213T190000" nil nil nil 2)))
    (save-excursion
      (if (re-search-forward "DTEND\\(;.*\\)?:\\(20121223T200000Z\\)" nil t)
	  (replace-match "20121213T190000Z" nil nil nil 2)
	(re-search-forward "DTEND\\(;.*\\)?:\\(20121223T210000\\)")
	(replace-match "20121213T200000" nil nil nil 2)))
    (save-excursion
      (when (re-search-forward "SEQUENCE:\\s-*\\([0-9]+\\)" nil t)
	(replace-match (number-to-string
			(1+ (string-to-number (match-string 1))))
		       nil t nil 1)))
    (message "PUTting second changed event")
    (should (org-caldav-save-resource
	     (concat (org-caldav-events-url) "orgcaldavtest@org1.ics")
	     (encode-coding-string (buffer-string) 'utf-8))))

  ;; Aaaand sync!
  (message "4th SYNC")
  (org-caldav-sync)

  (should (equal `((,org-caldav-calendar-id "orgcaldavtest@cal1" changed-in-cal cal->org)
		   (,org-caldav-calendar-id "orgcaldavtest@org1" changed-in-cal cal->org))
		 org-caldav-sync-result))

  (with-current-buffer (find-file-noselect org-caldav-test-inbox)
    (goto-char (point-min))
    (should (re-search-forward
	     "* Changed test appointment Number 1
\\s-*:PROPERTIES:
\\s-*:ID:\\s-*orgcaldavtest@cal1
\\s-*:END:
\\s-*<2012-12-12 Wed>
\\s-*A first test")))

  (message "Deleting event in Org")
  (with-current-buffer (find-file-noselect org-caldav-test-orgfile)
    (goto-char (point-min))
    (should (re-search-forward
	     "* This is a changed test
\\s-*:PROPERTIES:
\\s-*:ID:\\s-*orgcaldavtest@org1
\\s-*:END:
\\s-*<2012-12-13 Thu 19:00-20:00>
\\s-*Foo Bar Baz"))
    ;; Delete this event in Org
    (replace-match ""))

  ;; Sync
  (message "6th SYNC")
  (org-caldav-sync)

  ;; Event should be deleted in calendar
  (let ((calevents (org-caldav-get-event-etag-list)))
    (should (= (length calevents) 3))
    (should-not (assoc '"orgcaldavtest@org1" calevents)))
  (should
   (equal org-caldav-sync-result
	  `((,org-caldav-calendar-id "orgcaldavtest@org1" deleted-in-org removed-from-cal))))
  (should-not
   (assoc '"orgcaldavtest@org1" org-caldav-event-list))

  ;; Delete event in calendar
  (message "Delete event in calendar")
  (should (org-caldav-delete-event "orgcaldavtest-org2"))
  ;; Sync one last time
  (message "7th SYNC")
  (let ((org-caldav-delete-org-entries 'always))
    (org-caldav-sync))

  (should
   (equal org-caldav-sync-result
  	  `((,org-caldav-calendar-id "orgcaldavtest-org2" deleted-in-cal removed-from-org))))
  ;; There shouldn't be anything left in that buffer
  (with-current-buffer (find-file-noselect org-caldav-test-orgfile)
    (goto-char (point-min))
    (should-not (re-search-forward "[:alnum:]" nil t)))
  (should-not
   (assoc '"orgcaldavtest-org2" org-caldav-event-list))

  (org-caldav-test-cleanup)

  )

(ert-deftest org-caldav-02-change-heading-test ()
  (with-current-buffer (get-buffer-create "headingtest")
    (erase-buffer)
    (insert "* This is a test without timestamp in headline\n"
	    "  <2009-08-08 Sat 10:00>\n whatever\n foo\n bar\n")
    (insert "* This is a test SCHEDULED: <2009-08-08 Sat> \n"
	    "  whatever\n foo\n bar\n")
    (insert "*  <2009-08-08 Sat 14:00>  This is another test\n"
	    "  whatever\n foo\n bar\n")
    (org-mode)
    (goto-char (point-min))
    (save-excursion
      (org-caldav-change-heading "first changed heading"))
    (should (looking-at "^\\* first changed heading$"))
    (search-forward "*" nil t 2)
    (beginning-of-line)
    (save-excursion
      (org-caldav-change-heading "second changed heading"))
    (should (looking-at "^\\* second changed heading SCHEDULED: <2009-08-08 Sat> $"))
    (search-forward "*" nil t 2)
    (beginning-of-line)
    (save-excursion
      (org-caldav-change-heading "third changed heading"))
    (should (looking-at "^\\*  <2009-08-08 Sat 14:00> third changed heading\n"))
    ))

(ert-deftest org-caldav-03-insert-org-entry ()
  "Make sure that `org-caldav-insert-org-entry' works fine."
  (let ((entry '("01 01 2015" "19:00" "01 01 2015" "20:00" "The summary" "The description" "location" nil))
        (org-caldav-select-tags ""))
    (cl-flet ((write-entry (uid level)
                           (with-temp-buffer
                             (org-mode) ; useful to set org-mode's
                                        ; internal variables
                             (apply #'org-caldav-insert-org-entry
                                    (append entry (list uid level)))
                             (setq foo (buffer-string)))))
      (should (string-match (concat
			     "\\*\\s-+The summary\n"
			     "\\s-*:PROPERTIES:\n"
			     "\\s-*:LOCATION: location\n"
			     "\\s-*:END:\n"
			     "\\s-*<2015-01-01 Thu 19:00-20:00>\n"
			     "\\s-*The description\n")
			    (write-entry nil nil)))
      (should (string-match (concat
			     "\\*\\*\\s-+The summary\n"
			     "\\s-*:PROPERTIES:\n"
			     "\\s-*:LOCATION: location\n"
			     "\\s-*:END:\n"
			     "\\s-*<2015-01-01 Thu 19:00-20:00>\n"
			     "\\s-*The description\n")
			    (write-entry nil 2)))
      (should (string-match (concat "\\*\\s-+The summary\n"
				    "\\s-*:PROPERTIES:\n"
				    "\\s-*:ID:\\s-*1\n"
				    "\\s-*:LOCATION: location\n"
				    "\\s-*:END:\n"
				    "\\s-*<2015-01-01 Thu 19:00-20:00>\n"
				    "\\s-*The description\n")
			    (write-entry "1" nil))))))

(ert-deftest org-caldav-04-multiple-calendars ()
  (org-caldav-test-setup-temp-files)
  (with-current-buffer (find-file-noselect org-caldav-test-orgfile)
    (insert org-caldav-test-org1)
    (save-buffer))

  (with-current-buffer (find-file-noselect org-caldav-test-second-orgfile)
    (insert org-caldav-test-org2)
    (save-buffer))

  ;; Delete calendar contents
  (let ((org-caldav-calendar-id (car org-caldav-test-calendar-names)))
    (org-caldav-test-set-up))
  (let ((org-caldav-calendar-id (nth 1 org-caldav-test-calendar-names)))
    (org-caldav-test-set-up))
  
  (let
      ((org-caldav-calendars
	`((:calendar-id ,(car org-caldav-test-calendar-names)
			:url ,org-caldav-url
			:files (,org-caldav-test-orgfile)
			:inbox ,org-caldav-test-orgfile)
	  (:calendar-id ,(nth 1 org-caldav-test-calendar-names)
			:url ,org-caldav-url
			:files (,org-caldav-test-second-orgfile)
			:inbox ,org-caldav-test-second-orgfile))))
    (org-caldav-sync))

  ;; Check that each calendar has one event
  (let
      ((org-caldav-calendar-id (car org-caldav-test-calendar-names)))
    (should (org-caldav-get-event "orgcaldavtest@org1"))
    (should-error (org-caldav-get-event "orgcaldavtest-org2")))

  (let
      ((org-caldav-calendar-id (nth 1 org-caldav-test-calendar-names)))
    (should-error (org-caldav-get-event "orgcaldavtest@org1"))
    (should (org-caldav-get-event "orgcaldavtest-org2")))

  (org-caldav-test-cleanup)
  )

(ert-deftest org-caldav-05-multiple-calendars-agenda-skip-function ()
  (org-caldav-test-setup-temp-files)
  (with-current-buffer (find-file-noselect org-caldav-test-orgfile)
    (insert org-caldav-test-org1)
    (insert org-caldav-test-org2)
    (insert org-caldav-test-org3)
    (save-buffer))

  ;; Delete calendar contents
  (let ((org-caldav-calendar-id (car org-caldav-test-calendar-names)))
    (org-caldav-test-set-up))
  (let ((org-caldav-calendar-id (nth 1 org-caldav-test-calendar-names)))
    (org-caldav-test-set-up))
  (message "Starting sync")
  (let
      ((org-caldav-calendars
	`((:calendar-id ,(car org-caldav-test-calendar-names)
			:url ,org-caldav-url
			:skip-conditions  (regexp ":sometag:")
			:files (,org-caldav-test-orgfile)
			:inbox ,org-caldav-test-orgfile)
	  (:calendar-id ,(nth 1 org-caldav-test-calendar-names)
			:url ,org-caldav-url
			:skip-conditions (notregexp ":sometag:")
			:files (,org-caldav-test-orgfile)
			:inbox ,org-caldav-test-orgfile))))
    (org-caldav-sync))

  ;; Check that each calendar has one event
  (let
      ((org-caldav-calendar-id (car org-caldav-test-calendar-names)))
    (should (org-caldav-get-event "orgcaldavtest@org1"))
    (should (org-caldav-get-event "orgcaldavtest-org2"))
    (should-error (org-caldav-get-event "orgcaldavtest-org3")))

  (let
      ((org-caldav-calendar-id (nth 1 org-caldav-test-calendar-names)))
    (should (org-caldav-get-event "orgcaldavtest-org3"))
    (should-error (org-caldav-get-event "orgcaldavtest@org1"))
    (should-error (org-caldav-get-event "orgcaldavtest-org2")))

  ;; Make sure org-agenda-skip-function-global is not set permanently
  (should-not org-agenda-skip-function-global)

  (org-caldav-test-cleanup)
  )

;; Make sure setting org-caldav-files to 'nil' does not
;; do anything weird.
(ert-deftest org-caldav-06-org-caldav-files-nil ()
  (message "Setting up temporary files")
  (org-caldav-test-setup-temp-files)
  (setq org-caldav-calendar-id (car org-caldav-test-calendar-names))
  ;; Set org-caldav-files to nil
  (setq org-caldav-files nil)
  (setq org-caldav-inbox org-caldav-test-inbox)
  (setq org-caldav-debug-level 2)
  (message "Setting up upstream calendar")
  (org-caldav-test-set-up)
  (message "Putting events")
  (org-caldav-test-put-events)
  (org-caldav-sync)
  ;; Events must still be in calendar
  (should (org-caldav-get-event "orgcaldavtest@cal1"))
  (should (org-caldav-get-event "orgcaldavtest-cal2"))
  ;; Sync result
  (should (or (equal org-caldav-test-sync-result
	             org-caldav-sync-result)
              (equal (reverse org-caldav-test-sync-result)
	             org-caldav-sync-result)))
  (org-caldav-test-cleanup))

;; Check that we are able to detect when an Org file was removed from
;; org-caldav-files between syncs.
(ert-deftest org-caldav-07-detect-removed-file ()
  (message "Setting up temporary files")
  (org-caldav-test-setup-temp-files)
  (with-current-buffer (find-file-noselect org-caldav-test-orgfile)
    (insert org-caldav-test-org1)
    (save-buffer))
  (with-current-buffer (find-file-noselect org-caldav-test-second-orgfile)
    (insert org-caldav-test-org2)
    (save-buffer))
  (setq org-caldav-calendar-id (car org-caldav-test-calendar-names))
  ;; Set org-caldav-files to nil
  (setq org-caldav-files (list org-caldav-test-orgfile org-caldav-test-second-orgfile))
  (setq org-caldav-inbox org-caldav-test-inbox)
  (setq org-caldav-debug-level 2)
  (message "Setting up upstream calendar")
  (org-caldav-test-set-up)
  (message "Putting events")
  (org-caldav-test-put-events)
  (message "1st sync")
  (org-caldav-sync)
  ;; Remove one of the files
  (setq org-caldav-files (list org-caldav-test-second-orgfile))
  ;; Sync again, binding yes-or-no-p to our test
  (setq org-caldav-test-seen-prompt nil)
  (let (octest-seen-prompt)
    (cl-letf (((symbol-function 'yes-or-no-p)
	       (lambda (prompt)
		 (setq octest-seen-prompt prompt) nil)))
      (message "2nd sync")
      (should-error (org-caldav-sync))
      (should (string-match "WARNING: Previously synced"
			    octest-seen-prompt))))
  (org-caldav-test-cleanup))

(ert-deftest org-caldav-test-multiline-location ()
  (with-temp-buffer
    (org-mode)
    (insert org-caldav-test-org1)
    (goto-char (point-min))
    (let ((orig-id (alist-get "ID" (org-entry-properties) nil nil #'string=)))
      (org-caldav-change-location "multi\nline")
      (let ((props (org-entry-properties)))
	(should (string= (alist-get "ID" props nil nil #'string=) orig-id))
	(should (string-match-p "multi" (alist-get "LOCATION" props nil nil #'string=)))
	(should (string-match-p "line" (alist-get "LOCATION" props nil nil #'string=)))))))

(ert-deftest org-caldav-08-test-setting-sync-direction ()
  (org-caldav-test-setup-temp-files)
  (with-current-buffer (find-file-noselect org-caldav-test-orgfile)
    (insert org-caldav-test-org1)
    (insert org-caldav-test-org2)
    (insert org-caldav-test-org3)
    (save-buffer))

  ;; Delete calendar contents
  (let ((org-caldav-calendar-id (car org-caldav-test-calendar-names)))
    (org-caldav-test-set-up))
  (let ((org-caldav-calendar-id (nth 1 org-caldav-test-calendar-names)))
    (org-caldav-test-set-up))
  (message "Starting sync")
  (let
      ((org-caldav-calendars
	;; First only syncs Org to calendar
	`((:calendar-id ,(car org-caldav-test-calendar-names)
			:url ,org-caldav-url
			:files (,org-caldav-test-orgfile)
			:inbox nil ;; No inbox needed
			:sync-direction org->cal)
	  ;; Second only syncs Calendar to Org inbox
	  (:calendar-id ,(nth 1 org-caldav-test-calendar-names)
			:url ,org-caldav-url
			:files nil ;; No files needed
			:inbox ,org-caldav-test-inbox
			:sync-direction cal->org))))
    (org-caldav-sync)

    ;; First calendar should sync everything
    (let
	((org-caldav-calendar-id (car org-caldav-test-calendar-names)))
      (should (org-caldav-get-event "orgcaldavtest@org1"))
      (should (org-caldav-get-event "orgcaldavtest-org2"))
      (should (org-caldav-get-event "orgcaldavtest-org3")))

    ;; Second calendar syncs nothing from org to cal
    (let
	((org-caldav-calendar-id (nth 1 org-caldav-test-calendar-names)))
      (should-error (org-caldav-get-event "orgcaldavtest-org3"))
      (should-error (org-caldav-get-event "orgcaldavtest@org1"))
      (should-error (org-caldav-get-event "orgcaldavtest-org2")))

    ;; Put calendar events in both calendars
    (let ((org-caldav-calendar-id (car org-caldav-test-calendar-names)))
      (org-caldav-test-put-events))
    (let ((org-caldav-calendar-id (nth 1 org-caldav-test-calendar-names)))
      (org-caldav-test-put-events))
    ;; Sync again
    (org-caldav-sync)

    ;; Events are still there in first
    (let
	((org-caldav-calendar-id (car org-caldav-test-calendar-names)))
      (should (org-caldav-get-event "orgcaldavtest@org1"))
      (should (org-caldav-get-event "orgcaldavtest-org2"))
      (should (org-caldav-get-event "orgcaldavtest-org3")))
    ;; Events still not there in second
    (let
	((org-caldav-calendar-id (nth 1 org-caldav-test-calendar-names)))
      (should-error (org-caldav-get-event "orgcaldavtest-org3"))
      (should-error (org-caldav-get-event "orgcaldavtest@org1"))
      (should-error (org-caldav-get-event "orgcaldavtest-org2")))
    ;; But second should have new events in inbox
    (with-current-buffer (find-file-noselect org-caldav-test-inbox)
      (goto-char (point-min))
      (should (re-search-forward org-caldav-test-ics1-org nil t))
      (goto-char (point-min))
      (should (re-search-forward org-caldav-test-ics2-org nil t))))

  (org-caldav-test-cleanup)
)

;; Based on org-caldav-01-sync-test, but modified for todos
(ert-deftest org-caldav-09-sync-test-todo ()
  (let ((org-caldav-sync-todo t)
        (org-icalendar-include-todo 'all))
    (message "Setting up temporary files")
    (org-caldav-test-setup-temp-files)
    (setq org-caldav-calendar-id (car org-caldav-test-calendar-names))
    ;; Set up data for org-caldav.
    (setq org-caldav-files (list org-caldav-test-orgfile))
    (setq org-caldav-inbox org-caldav-test-inbox)
    (setq org-caldav-debug-level 2)

    (message "Cleaning up upstream calendars")
    (org-caldav-test-set-up)

    ;; Set up orgfile.
    (with-current-buffer (find-file-noselect org-caldav-test-orgfile)
      (insert org-caldav-test-org4 "\n")
      (insert org-caldav-test-org5 "\n")
      (save-buffer))

    (message "Putting events")
    (let ((org-caldav-calendar-preamble org-caldav-test-preamble)
          events)
      (with-temp-buffer
        (insert org-caldav-test-ics3)
        (should (org-caldav-put-event (current-buffer)))
        (erase-buffer)
        (insert org-caldav-test-ics4)
        (should (org-caldav-put-event (current-buffer))))
      (let ((events (org-caldav-get-event-etag-list)))
        (should (assoc "orgcaldavtest@cal3" events))
        (should (assoc "orgcaldavtest-cal4" events))))

    (message "1st SYNC")
    ;; Do the sync.
    (org-caldav-sync)
    ;;;; Check result.
    (should (= (length (org-caldav-get-event-etag-list)) 4))
    (should (member `(,org-caldav-calendar-id "orgcaldavtest@cal3" new-in-cal cal->org)
        	    org-caldav-sync-result))
    (should (member `(,org-caldav-calendar-id "orgcaldavtest-cal4" new-in-cal cal->org)
        	    org-caldav-sync-result))
    (should (member `(,org-caldav-calendar-id "orgcaldavtest@org4" new-in-org org->cal)
        	    org-caldav-sync-result))
    (should (member `(,org-caldav-calendar-id "orgcaldavtest-org5" new-in-org org->cal)
        	    org-caldav-sync-result))
    ;; State file should exist now.
    (should (file-exists-p
	     (org-caldav-sync-state-filename org-caldav-calendar-id)))
    (let ((calevents (org-caldav-get-event-etag-list)))
      (should (= (length calevents) (length org-caldav-test-alltodos)))
      ;; Org events should be in cal.
      (dolist (cur org-caldav-test-alltodos)
        (should (assoc cur calevents))))
    ;; Cal events should be in Org.
    (with-current-buffer (find-file-noselect org-caldav-test-inbox)
      (goto-char (point-min))
      (should (re-search-forward org-caldav-test-ics3-org nil t))
      (goto-char (point-min))
      (should (re-search-forward org-caldav-test-ics4-org nil t)))

    (message "2nd SYNC")
    ;; Sync again.
    (org-caldav-sync)
    ;; Nothing should have happened.
    (should-not org-caldav-sync-result)

  (message "Changing org events")
  ;; Now change events in Org
  (with-current-buffer (find-buffer-visiting org-caldav-test-orgfile)
    (goto-char (point-min))
    (search-forward "TODO A test task from Org")
    (replace-match "DONE Finished test task from Org")
    (search-forward "SCHEDULED:")
    (replace-match "CLOSED: [2012-12-24 Mon 00:00] SCHEDULED:" t))
  (with-current-buffer (find-buffer-visiting org-caldav-test-inbox)
    (goto-char (point-min))
    (search-forward "TODO [#B] Another test task from iCal")
    (replace-match "DONE [#C] Another test task from iCal was finished!")
    (search-forward "DEADLINE:")
    (replace-match "CLOSED: [2012-12-20 Thu 00:00] DEADLINE:" t))

  ;; And sync...
  (message "3rd SYNC")
  (org-caldav-sync)
  (should (equal `((,org-caldav-calendar-id "orgcaldavtest-cal4" changed-in-org org->cal)
		   (,org-caldav-calendar-id "orgcaldavtest@org4" changed-in-org org->cal))
		 org-caldav-sync-result))

  ;; Check if those events correctly end up in calendar.
  (with-current-buffer (org-caldav-get-event "orgcaldavtest-cal4")
    (goto-char (point-min))
    (save-excursion
      (should (search-forward "SUMMARY:Another test task from iCal was finished!")))
    (save-excursion
      (should (search-forward "PRIORITY:9")))
    (save-excursion
      (should (search-forward "STATUS:COMPLETED")))
    (save-excursion
      (should (re-search-forward "COMPLETED.*:20121220T000000"))))

  (with-current-buffer (org-caldav-get-event "orgcaldavtest@org4")
    (goto-char (point-min))
    (save-excursion
      (should (search-forward "SUMMARY:Finished test task from Org")))
    (save-excursion
      (should (search-forward "PRIORITY:0")))
    (save-excursion
      (should (search-forward "STATUS:COMPLETED")))
    (save-excursion
      (should (re-search-forward "COMPLETED.*:20121224T000000"))))

  ;; Now change events in Cal
  (message "Changing events in calendar")
  (with-current-buffer (org-caldav-get-event "orgcaldavtest@cal3")
    (goto-char (point-min))
    (save-excursion
      (search-forward "SUMMARY:A test task from iCal")
      (replace-match "SUMMARY:Changed A test task from iCal"))
    (save-excursion
      (re-search-forward "DTSTART\\(;.*\\)?:\\(20121223\\)")
      (replace-match "20121224" nil nil nil 2))
    (save-excursion
      (when (re-search-forward "SEQUENCE:\\s-*\\([0-9]+\\)" nil t)
	(replace-match (number-to-string
			(1+ (string-to-number (match-string 1))))
		       nil t nil 1)))
    (message "PUTting first changed event")
    (should (org-caldav-save-resource
	     (concat (org-caldav-events-url) (url-hexify-string "orgcaldavtest@cal3.ics"))
	     (encode-coding-string (buffer-string) 'utf-8))))

  (with-current-buffer (org-caldav-get-event "orgcaldavtest-org5")
    (goto-char (point-min))
    (save-excursion
      (search-forward "SUMMARY:Another test task from Org")
      (replace-match "SUMMARY:Changed Another test task from Org"))
    (save-excursion
      (search-forward "STATUS:NEEDS-ACTION")
      (replace-match "STATUS:COMPLETED\nCOMPLETED:20121224T000000"))
    (save-excursion
      (search-forward "PERCENT-COMPLETE:0")
      (replace-match "PERCENT-COMPLETE:100"))
    (save-excursion
      (when (re-search-forward "SEQUENCE:\\s-*\\([0-9]+\\)" nil t)
        (replace-match (number-to-string
        		(1+ (string-to-number (match-string 1))))
        	       nil t nil 1)))
    (message "PUTting second changed event")
    (should (org-caldav-save-resource
             (concat (org-caldav-events-url) "orgcaldavtest-org5.ics")
             (encode-coding-string (buffer-string) 'utf-8))))

  ;; Aaaand sync!
  (message "4th SYNC")
  (org-caldav-sync)

  (should (equal `((,org-caldav-calendar-id "orgcaldavtest@cal3" changed-in-cal cal->org)
		   (,org-caldav-calendar-id "orgcaldavtest-org5" changed-in-cal cal->org))
		 org-caldav-sync-result))

  (with-current-buffer (find-file-noselect org-caldav-test-inbox)
    (goto-char (point-min))
    (should (re-search-forward
	     "* TODO Changed A test task from iCal
\\s-*SCHEDULED: <2012-12-24 Mon>
\\s-*:PROPERTIES:
\\s-*:ID:\\s-*orgcaldavtest@cal3
\\s-*:END:
\\s-*ical test task 1")))

  (message "Changing description in icalendar")
  (with-current-buffer (org-caldav-get-event "orgcaldavtest@cal3")
    (goto-char (point-min))
    (save-excursion
      (search-forward "DESCRIPTION:ical test task 1")
      (replace-match "DESCRIPTION:ical test task 1 modified"))
    
    (message "PUTting changed description")
    (should (org-caldav-save-resource
	     (concat (org-caldav-events-url) (url-hexify-string "orgcaldavtest@cal3.ics"))
	     (encode-coding-string (buffer-string) 'utf-8))))

  (message "5th SYNC")
  (let ((org-caldav-sync-changes-to-org 'all))
    (org-caldav-sync))
  
  (should (equal `((,org-caldav-calendar-id "orgcaldavtest@cal3"
                                            changed-in-cal cal->org))
		 org-caldav-sync-result))

  (with-current-buffer (find-file-noselect org-caldav-test-inbox)
    (goto-char (point-min))
    (should (re-search-forward
	     "* TODO Changed A test task from iCal
\\s-*SCHEDULED: <2012-12-24 Mon>
\\s-*:PROPERTIES:
\\s-*:ID:\\s-*orgcaldavtest@cal3
\\s-*:END:
\\s-*ical test task 1 modified")))

  (message "Deleting event in Org")
  (with-current-buffer (find-file-noselect org-caldav-test-orgfile)
    (goto-char (point-min))
    (message (buffer-string))
    (should (search-forward
	     "* DONE [#B] Changed Another test task from Org                         :test:
CLOSED: [2012-12-24 Mon 00:00] DEADLINE: <2012-12-23 Sun> SCHEDULED: <2012-12-19 Wed>
:PROPERTIES:
:ID:       orgcaldavtest-org5
:END:
Org task 2

"))
    ;; Delete this event in Org
    (replace-match ""))

  ;; Sync
  (message "6th SYNC")
  (org-caldav-sync)

  ;; Event should be deleted in calendar
  (let ((calevents (org-caldav-get-event-etag-list)))
    (should (= (length calevents) 3))
    (should-not (assoc '"orgcaldavtest-org5" calevents)))
  (should
   (equal org-caldav-sync-result
	  `((,org-caldav-calendar-id "orgcaldavtest-org5" deleted-in-org removed-from-cal))))
  (should-not
   (assoc '"orgcaldavtest-org5" org-caldav-event-list))

  ;; Delete event in calendar
  (message "Delete event in calendar")
  (should (org-caldav-delete-event "orgcaldavtest@org4"))
  ;; Sync one last time
  (message "7th SYNC")
  (let ((org-caldav-delete-org-entries 'always))
    (org-caldav-sync))

  (should
   (equal org-caldav-sync-result
  	  `((,org-caldav-calendar-id "orgcaldavtest@org4" deleted-in-cal removed-from-org))))
  ;; There shouldn't be anything left in that buffer
  (with-current-buffer (find-file-noselect org-caldav-test-orgfile)
    (goto-char (point-min))
    (should-not (re-search-forward "[:alnum:]" nil t)))
  (should-not
   (assoc '"orgcaldavtest@org4" org-caldav-event-list))

  (org-caldav-test-cleanup)))
