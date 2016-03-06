;; Test suite for org-caldav.el
;; Copyright, authorship, license: see org-caldav.el.

;; Run it from the org-caldav directory like this:
;; emacs -Q -L . --eval '(setq org-caldav-url "CALDAV-URL" org-caldav-calendar-id "CAL-ID")' -l org-caldav-testsuite.el -f ert

;; This will completely wipe the named calendar!

(require 'ert)
(require 'org)
(require 'org-caldav)

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
\\s-*A first test
\\s-*<2012-12-20 Thu>")

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
\\s-*A second test
\\s-*<2012-12-05 Wed 19:00-20:00>")

;; First test entry in Org which should end up in calendar
(setq org-caldav-test-org1
      "* This is a test
:PROPERTIES:
:ID:       orgcaldavtest@org1
:END:
<2012-12-23 Sun 20:00-21:00>
Foo Bar Baz")

;; Dto., second one
(setq org-caldav-test-org2
      "* This is another test
:PROPERTIES:
:ID:       orgcaldavtest-org2
:END:
<2012-12-19 Wed 19:00-21:00>
Baz Bar Foo")

;; All events after sync.
(setq org-caldav-test-allevents
      '("orgcaldavtest@org1" "orgcaldavtest-org2" "orgcaldavtest@cal1" "orgcaldavtest-cal2"))

;; Test files.
(setq org-caldav-test-orgfile "/tmp/org-caldav-test-orgfile.org")
(setq org-caldav-test-inbox "/tmp/org-caldav-test-inbox.org")

(when (file-exists-p org-caldav-test-orgfile)
  (delete-file org-caldav-test-orgfile))
(when (file-exists-p org-caldav-test-inbox)
  (delete-file org-caldav-test-inbox))

;; Set up inbox.
(with-current-buffer (find-file-noselect org-caldav-test-inbox)
  (save-buffer))

;; Set up orgfile.
(with-current-buffer (find-file-noselect org-caldav-test-orgfile)
  (insert org-caldav-test-org1 "\n")
  (insert org-caldav-test-org2 "\n")
  (save-buffer))

;; Set up data for org-caldav.
(setq org-caldav-files (list org-caldav-test-orgfile))
(setq org-caldav-inbox org-caldav-test-inbox)
(setq org-caldav-debug-level 2)

(message "Calendar URL: %s" org-caldav-url)
(message "Calendar ID: %s" org-caldav-calendar-id)

;; Remove state.
(when (file-exists-p
       (org-caldav-sync-state-filename org-caldav-calendar-id))
  (message "Removing state file")
  (delete-file (org-caldav-sync-state-filename org-caldav-calendar-id)))

(defun org-caldav-test-calendar-empty-p ()
  "Check if calendar is empty."
  (let ((output (url-dav-get-properties
		 (org-caldav-events-url)
		 '(DAV:resourcetype) 1)))
    (unless (eq (plist-get (cdar output) 'DAV:status) 200)
      (error "Could not query CalDAV URL %s." (org-caldav-events-url)))
    (= (length output) 1)))

(defun org-caldav-test-set-up ()
  "Make a clean slate."
  (message "SET UP")
  (unless (or (org-caldav-test-calendar-empty-p)
	      (not (y-or-n-p "Deleting everything in calendar. OK? ")))
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
      (org-caldav-put-event (current-buffer))
      (erase-buffer)
      (insert org-caldav-test-ics2)
      (org-caldav-put-event (current-buffer)))
    (should
     (setq events
	   (org-caldav-get-event-etag-list)))
    (should (assoc "orgcaldavtest@cal1" events))
    (should (assoc "orgcaldavtest-cal2" events))))

;; This is one, big, big test, since pretty much everything depends on
;; the current calendar/org state and I cannot easily split it.
(ert-deftest org-caldav-sync-test ()
  (org-caldav-test-set-up)
  (org-caldav-test-put-events)
  (message "SYNC")
  ;; Do the sync.
  (org-caldav-sync)
  ;; Check result.
  (should (member '("orgcaldavtest@cal1" new-in-cal cal->org)
		  org-caldav-sync-result))
  (should (member '("orgcaldavtest-cal2" new-in-cal cal->org)
		  org-caldav-sync-result))
  (should (member '("orgcaldavtest@org1" new-in-org org->cal)
		  org-caldav-sync-result))
  (should (member '("orgcaldavtest-org2" new-in-org org->cal)
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

  ;; Sync again.
  (org-caldav-sync)
  ;; Nothing should have happened.
  (should-not org-caldav-sync-result)

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
  (org-caldav-sync)
  (should (equal '(("orgcaldavtest-cal2" changed-in-org org->cal)
		   ("orgcaldavtest-org2" changed-in-org org->cal))
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
  (with-current-buffer (org-caldav-get-event "orgcaldavtest@cal1")
    (goto-char (point-min))
    (save-excursion
      (search-forward "SUMMARY:Test appointment Number 1")
      (replace-match "SUMMARY:Changed test appointment Number 1"))
    (save-excursion
      (search-forward "DTSTART;VALUE=DATE:20121220")
      (replace-match "DTSTART;VALUE=DATE:20121212"))
    (save-excursion
      (search-forward "DTEND;VALUE=DATE:20121221")
      (replace-match "DTEND;VALUE=DATE:20121213"))
    (save-excursion
      (when (re-search-forward "SEQUENCE:\\s-*\\([0-9]+\\)" nil t)
	(replace-match (number-to-string
			(1+ (string-to-number (match-string 1))))
		       nil t nil 1)))
    (url-dav-save-resource
     (concat (org-caldav-events-url) (url-hexify-string "orgcaldavtest@cal1.ics"))
     (encode-coding-string (buffer-string) 'utf-8)
     "text/calendar; charset=UTF-8"))

  (with-current-buffer (org-caldav-get-event "orgcaldavtest@org1")
    (goto-char (point-min))
    (save-excursion
      (search-forward "SUMMARY:This is a test")
      (replace-match "SUMMARY:This is a changed test"))
    (save-excursion
      (if (search-forward "DTSTART:20121223T190000Z" nil t)
	  (replace-match "DTSTART:20121213T180000Z")
	(search-forward "DTSTART:20121223T200000")
	(replace-match "DTSTART:20121213T190000")))
    (save-excursion
      (if (search-forward "DTEND:20121223T200000Z" nil t)
	  (replace-match "DTEND:20121213T190000Z")
	(search-forward "DTEND:20121223T210000")
	(replace-match "DTEND:20121213T200000")))
    (save-excursion
      (when (re-search-forward "SEQUENCE:\\s-*\\([0-9]+\\)" nil t)
	(replace-match (number-to-string
			(1+ (string-to-number (match-string 1))))
		       nil t nil 1)))
    (url-dav-save-resource
     (concat (org-caldav-events-url) "orgcaldavtest@org1.ics")
     (encode-coding-string (buffer-string) 'utf-8)
     "text/calendar; charset=UTF-8"))

  ;; Aaaand sync!
  (org-caldav-sync)

  (should (equal '(("orgcaldavtest@cal1" changed-in-cal cal->org)
		   ("orgcaldavtest@org1" changed-in-cal cal->org))
		 org-caldav-sync-result))

  (with-current-buffer (find-file-noselect org-caldav-test-inbox)
    (goto-char (point-min))
    (should (re-search-forward
	     "* Changed test appointment Number 1
\\s-*:PROPERTIES:
\\s-*:ID:\\s-*orgcaldavtest@cal1
\\s-*:END:
\\s-*A first test
\\s-*<2012-12-12 Wed>")))

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
  (org-caldav-sync)

  ;; Event should be deleted in calendar
  (let ((calevents (org-caldav-get-event-etag-list)))
    (should (= (length calevents) 3))
    (should-not (assoc '"orgcaldavtest@org1" calevents)))
  (should
   (equal org-caldav-sync-result
	  '(("orgcaldavtest@org1" deleted-in-org removed-from-cal))))
  (should-not
   (assoc '"orgcaldavtest@org1" org-caldav-event-list))

  ;; Delete event in calendar
  (org-caldav-delete-event "orgcaldavtest-org2")
  ;; Sync one last time
  (org-caldav-sync)

  (should
   (equal org-caldav-sync-result
  	  '(("orgcaldavtest-org2" deleted-in-cal removed-from-org))))
  ;; There shouldn't be anything left in that buffer
  (with-current-buffer (find-file-noselect org-caldav-test-orgfile)
    (goto-char (point-min))
    (should-not (re-search-forward "[:alnum:]" nil t)))
  (should-not
   (assoc '"orgcaldavtest-org2" org-caldav-event-list))

  )

(ert-deftest org-caldav-change-heading-test ()
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

(ert-deftest org-caldav-insert-org-entry ()
  "Make sure that `org-caldav-insert-org-entry' works fine."
  (let ((entry '("01 01 2015" "19:00" "01 01 2015" "20:00" "The summary" "The description"))
        (org-caldav-select-tags ""))
    (cl-flet ((write-entry (uid level)
                           (with-temp-buffer
                             (org-mode) ; useful to set org-mode's
                                        ; internal variables
                             (apply #'org-caldav-insert-org-entry
                                    (append entry (list uid level)))
                             (buffer-string))))
      (should (string= "* The summary\n<2015-01-01 Thu 19:00-20:00>\nThe description\n"
                       (write-entry nil nil)))
      (should (string= "** The summary\n<2015-01-01 Thu 19:00-20:00>\nThe description\n"
                       (write-entry nil 2)))
      (should (string= "* The summary\n  :PROPERTIES:\n  :ID:       1\n  :END:\n<2015-01-01 Thu 19:00-20:00>\nThe description\n"
                       (write-entry "1" nil))))))
