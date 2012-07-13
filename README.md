org-caldav
==========

Caldav sync for Emacs orgmode

Tested CalDAV servers: Owncloud, Google Calendar.

*IMPORTANT*: Do NOT simply put your main calendar in org-caldav-calendar-id!

Instead, create a new, dedicated calendar.  The code is still pretty
rough and might easily delete entries it should not delete.

This package depends on the url-dav package, which unfortunately is
broken in Emacs proper. Get a fixed one from
https://github.com/dengste/org-caldav and load it before using
org-caldav.

In a nutshell:

* Create a new calendar; the name does not matter. Again, do *not*
  use your precious main calendar.

* Set org-caldav-url to the base address of your CalDAV server:
    - Owncloud: https://OWNCLOUD-SERVER-URL/remote.php/caldav/calendars/USERID
    - Google: https://www.google.com/calendar/dav

* Set org-caldav-calendar-id to the calendar-id of your new calendar:
    - OwnCloud: Simply the name of the calendar.
    - Google: Click on 'calendar settings' and the id will be shown
      next to "Calendar Address". It is of the form
      ID@group.calendar.google.com. Do *not* omit the domain.

* Set org-caldav-files to the list of org files you would like to
  sync. Do NOT add the below org-caldav-inbox to this list or you'll
  get duplicates.  For everything else, you can use the
  org-icalendar-* variables, since org-caldav uses that package to
  generate the events.

* Set org-caldav-inbox to an org filename where new entries from
  the calendar should be stored.

Call org-caldav-sync to start the sync. The URL package will ask
you for username/password for accessing the calendar.

The syncing between org and the external calendar is not yet really
finished. Here's what the code can currently do:

* All items in org-caldav-files with active time stamps are synced to
  the calendar.

* If you change an item in one of those files in orgmode, this change
  will also get synced to the external calendar (to be exact: this will
  generate a new event and the old one will be deleted).

- If you create a *new* event in the calendar through Android or any
  other client (like browser), this event will land in the
  'org-caldav-inbox' file as an org item.

However:

* If you *change* an item directly on the CalDAV server, this will *not*
  get synced back to Org. What happens exactly depends on the server
  (whether it renames the event or not). Just try it out. The item might
  even get deleted after the next sync - you have been warned.

* Likewise, if you *change* and item in 'org-caldav-inbox', this will
  not get synced back to the calendar.
