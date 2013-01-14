# org-caldav

Caldav sync for Emacs Orgmode

Tested CalDAV servers: Owncloud, Google Calendar.

*IMPORTANT*: Before using this code, please make sure you have backups
of your precious Org files. Also, I strongly suggest to create a new,
empty calendar on your server for using this package. The code is
still pretty rough and might easily delete entries it should not
delete.

*ALSO IMPORTANT*: When using this package, all Org entries with an
active timestamp will get an UID property (see doc-string of
org-icalendar-store-UID for further details). If you don't want this,
then *do not use this package*; there is just no way around that. It is
the only reliable way to uniquely identify Org entries.

*ALSO ALSO IMPORTANT*: If you used earlier versions of this package from
2012, you will have to start from scratch.

This package depends on the url-dav package, which unfortunately is
broken in Emacs <=24.2. If you don't want to upgrade Emacs, you can
load the url-dav package from this github repository *before* using
org-caldav.

### IN A NUTSHELL

* Create a new calendar; the name does not matter.

* Set org-caldav-url to the base address of your CalDAV server:
    - Owncloud: https://OWNCLOUD-SERVER-URL/remote.php/caldav/calendars/USERID
    - Google: https://www.google.com/calendar/dav

* Set org-caldav-calendar-id to the calendar-id of your new calendar:
    - OwnCloud: Simply the name of the calendar.
    - Google: Click on 'calendar settings' and the id will be shown
      next to "Calendar Address". It is of the form
      ID@group.calendar.google.com. Do *not* omit the domain!

* Set org-caldav-inbox to an org filename where new entries from the
  calendar should be stored. Just to be safe, I suggest using an
  empty, dedicated Org file for that.

* Set org-caldav-files to the list of org files you would like to
  sync. The above org-caldav-inbox will be automatically added, so you
  don't have to add it here.

* It is usually a good idea to manually set org-icalendar-timezone to
  the timezone of your remote calendar. It should be a simple string
  like "Europe/Berlin". You can also take a look at the other
  org-icalendar variables, since org-caldav uses this package to
  export your entries.

Call org-caldav-sync to start the sync. The URL package will ask you
for username/password for accessing the calendar. (See below on how to
store that password in an authinfo file.)

The first sync can easily take several minutes, depending on the
number of calendar items. Especially Google's CalDAV interface is
pretty slow. If you have to abort the initial sync for some reason,
just start org-caldav-sync again in the same Emacs session and you
should get asked if you'd like to resume.

The same goes for sync errors you might get. Especially when using
Google Calendar, it is not unusual to get stuff like '409' errors
during the initial sync. Only Google knows why. Just run
org-caldav-sync again until all events are uploaded.

### DETAILS

Compared to earlier versions of this package from 2012, it now does
proper two-way syncing, that means it does not matter where and how
you change an entry. You can also move Org entries freely from one
file to another, as long as they are all listed in
org-caldav-files. The org-icalendar package will put a unique ID on
each entry with an active timestamp, so that org-caldav can find
it. It will also sync deletions, but more on that later.

#### Org and the iCalendar format

An Org entry can store much more information than an iCalendar entry,
so there is no one-to-one correspondence between the two formats which
makes syncing a bit difficult.

* Org to iCalendar

This package uses the org-icalendar package to do the export to the
iCalendar format (.ics files). By default, it uses the title of the
Org entry as SUMMARY and puts the entry's body into DESCRIPTION,
snipping stuff like properties and timestamps (you can override that
with properties of the same name, but IMO it makes stuff just more
complicated). The variable org-icalendar-include-body (100 by default)
denotes how many characters from the body should be included as
DESCRIPTION.

* iCalendar to Org

If you create a new iCalendar entry in your calendar, you'll get an
Org entry with SUMMARY as heading, DESCRIPTION as body and the
timestamp. However, if you *change* an existing entry in the calendar,
things get more complicated and the variable
org-caldav-sync-changes-to-org comes into play. Its default is the
symbol "title-and-timestamp", which means that only the entry's
heading is synced (with SUMMARY) and the timestamp gets updated, but
*not* the entry's body with DESCRIPTION.  The simple reason is that
you might loose data, since DESCRIPTION is rather limited in what it
can store. Still, you can set the variable to the symbol "all", which
will completely *replace* an existing Org entry with the entry that
gets generated from the calendar's event. You can also limit syncing
to heading and/or timestamp only.

To be extra safe, org-caldav will by default backup entries it
changes. See the variable org-caldav-backup-file for details.

#### Syncing deletions

If you delete entries in your Org files, the corresponding iCalendar
entries will simply get deleted.

If you delete events in your calendar, you will by default get asked
if you'd like to delete the corresponding Org event. You can change
that behavior through org-caldav-delete-org-entries.

If you answer a deletion request with "no", the event should get
re-synced to the calendar next time you call org-caldav-sync.

#### Conflict handling

Now that's an easy one: Org always wins. That means, if you change an
entry in Org *and* in the calendar, the changes in the calendar will
be lost. I might implement proper conflict handling some day, but
don't hold your breath (patches are welcome, of course).

#### Storing authentication information in authinfo/netrc

If you don't want to enter your user/password every time, you can
store it permanently in an authinfo file. In Emacs, the auth-source
package takes care of that, but the syntax for https authentication is
a bit peculiar. You have to use a line like the following

machine www.google.com:443 port https login username password secret

Note that you have to specify the port number in the URL and *also*
specify 'https' for the port. This is not a bug. For more information,
see (info "auth"), especially section "Help for users".

Since you are storing your password in a file it makes sense to
encrypt it using GnuPG. While Emacs supports this, there's a subtle
bug which makes this feature unusable for the URL package (see bug
11981). This was fixed in Emacs 24.2, so if you're using an older
version, you'll have to upgrade if you want encrypted authinfo files
for org-caldav.

#### Storage of sync information and sync from different computers

The current sync state is stored in a file org-caldav-<SOMEID>.el in
the ~/.emacs.d directory. You can change the location through the
variable org-caldav-save-directory. SOMEID directly depends on the
calendar id (it's a snipped MD5).

If you sync your Org files across different machines and want to use
org-caldav on all of them, don't forget to sync the org sync state,
too. Probably your best bet is to set org-caldav-save-directory to the
path you have your Org files in, so that it gets copied alongside with
them.

#### Starting from scratch

If your sync state somehow gets broken, you can make a clean slate by
doing

C-u M-x org-caldav-delete-everything

The function has to be called with a prefix so that you don't call it
by accident. This will delete everything in the calendar along with
the current sync state. You can then call org-caldav-sync afterwards
and it will completely re-sync with the now empty calendar. Deleting
many events can be slow; in that case, just delete the calendar and
re-create it, delete the sync state file in ~/.emacs.d and restart
Emacs.

#### Syncing with more than one calendar

This is not built-in, but it can be done. Since the sync state's
filename depends on the calendar-id, you can call org-caldav-sync more
than once with different values for org-caldav-files, org-caldav-inbox
and org-caldav-calendar-id. I admit this is cumbersome, and I probably
will someday add the feature to sync only items with certain tags to
certain calendars, but I'd like to get the basic functionality real
stable first.

#### Timezone problems

Timezone handling is plain horrible, and it seems every CalDAV server
does it slightly differently, also using non-standard headers like
X-WR-TIMEZONE. If you see items being shifted by a few hours, make
really really sure you have properly set org-icalendar-timezone and
that your calendar is configured to use the same one.

#### Known Bugs

* Recurring events created or changed on the calendar side cannot be
  synced (they will work fine as long as you manage them in Org,
  though).

* Syncing is currently pretty slow since everything is done
  synchronously.

* 'LOCATION' is ignored.


#### How syncing happens (a.k.a. my little CalDAV rant)

(This is probably not interesting, so you can just stop reading.)

CalDAV is a mess.

First off, it is based on WebDAV, which has its own fair share of
problems. The main design flaw of CalDAV however, is that UID and
ressource name (the "filename", if you want) are two different
things. I know that there are reasons for that (not everything has a
UID, like timezones, and you can put several events in one ressource),
but this is typical over-engineering to allow some marginal use cases
pretty much no one needs. Another problem is that you have to do
additional round-trips to get Etag and sequence number, which makes
CalDAV pretty slow.

Org-caldav takes the easy route: it assumes that every ressource
contains one event, and that UID and ressource name are identical. In
fact, Google's CalDAV interface even enforces the latter. And while
Owncloud does not enforce it, at least it just does it if you create
items in its web interface.

However, the CalDAV standard does not demand this, so I guess there
are servers out there with which org-caldav does not work. Patches
welcome.

Now, all this would be bad enough if it weren't for the sloppy server
implementations which make implementing a CalDAV client a living hell
and led to several rewrites of the code. Especially Google, the 500
pound gorilla in the room, doesn't really care much for CalDAV. I
guess they really like their own shiny REST-based calendar API better,
and I can't blame them for that.
