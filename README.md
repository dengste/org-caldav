# org-caldav

Caldav sync for Emacs Orgmode

**Minimum Emacs version needed**: 24.3

CalDAV servers:

* **Owncloud** and **Nextcloud**: Regularly tested.

* **Google Calendar**: Should work, but you need to register an
application with the Google Developer Console for OAuth2
authentication (see below).

* **Radicale** and **Baikal**: Should work. If you get problems with
'Digest' authentication, switch back to 'Basic' (make sure to use
https, though!). If you get asked for password repeatedly, put it in
.authinfo file (see below).

* **SOGo** and **Kolab**: Reported to be working
    (https://kolabnow.com/clients/emacs)

Note that current Emacs releases do not correctly handle https over a
proxy connection
(https://debbugs.gnu.org/cgi/bugreport.cgi?bug=11788). If you need
that, you'll have to use a recent Emacs 26.0.x snapshot.

**IMPORTANT**: Before using this code, please make sure you have backups
of your precious Org files. Also, I strongly suggest to create a new,
empty calendar on your server for using this package.

**ALSO IMPORTANT**: When using this package, possibly all Org entries
will get an UID property (see doc-string of org-icalendar-store-UID
for further details). If you don't want this, then *do not use this
package*; there is just no way around that. It is the only reliable
way to uniquely identify Org entries.

### IN A NUTSHELL

* Create a new calendar; the name does not matter.

* Set `org-caldav-url` to the base address of your CalDAV server:
    - Owncloud/Nextcloud (9.x and above): https://OWNCLOUD-SERVER-URL/remote.php/dav/calendars/USERID
    - Owncloud 8.x and below: https://OWNCLOUD-SERVER-URL/remote.php/caldav/calendars/USERID
    - Google: Set to symbol 'google. See below for further documentation.

* Set `org-caldav-calendar-id` to the calendar-id of your new
calendar:
    - Own/NextCloud: Click on that little symbol next to the calendar
      name and inspect the link of the calendar; the last element of
      the shown path is the calendar-id. This should *usually* be the
      same as the name of the calendar, but not necessarily: Owncloud
      might replace certain characters (upper- to lowercase, for
      instance), or it might even be entirely different if the
      calendar was created by another CalDAV application.
    - Google: Click on 'calendar settings' and the id will be shown
      next to "Calendar Address". It is of the form
      `ID@group.calendar.google.com`. Do *not* omit the domain!

* Set `org-caldav-inbox` to an org filename where new entries from the
  calendar should be stored. Just to be safe, I suggest using an
  empty, dedicated Org file for that.

* Set `org-caldav-files` to the list of org files you would like to
  sync. The above `org-caldav-inbox` will be automatically added, so you
  don't have to add it here.

* It is usually a good idea to manually set `org-icalendar-timezone` to
  the timezone of your remote calendar. It should be a simple string
  like "Europe/Berlin". If that doesn't work and your events are
  shifted by a few hours, try the setting "UTC" (the SOGo calendar
  server seems to need this).

Please note that org-caldav does not directly control how and which
entries are exported, it just uses the org-icalendar
exporter. Therefore, you should also take a look at the options from
the org-icalendar exporter. Most importantly, take a look at
`org-icalendar-alarm-time` to add a reminder to your entries, and
`org-icalendar-use-deadline` and `org-icalendar-use-scheduled` to control
which timestamps should be used.

Call `org-caldav-sync` to start the sync. The URL package will ask you
for username/password for accessing the calendar. (See below on how to
store that password in an authinfo file.)

The first sync can easily take several minutes, depending on the
number of calendar items. Especially Google's CalDAV interface is
pretty slow. If you have to abort the initial sync for some reason,
just start `org-caldav-sync` again in the same Emacs session and you
should get asked if you'd like to resume.

The same goes for sync errors you might get. Especially when using
Google Calendar, it is not unusual to get stuff like '409' errors
during the initial sync. Only Google knows why. Just run
`org-caldav-sync` again until all events are uploaded.

### Syncing to Google Calendar

The new CalDAV endpoint for Google Calendar requires OAuth2
authentication.  So first, you need to install the oauth2 library from
GNU ELPA, and afterwards you need to acquire an application ID and
secret from the Google Developer Console. For details on how to do
this, follow the Google documentation at

https://developers.google.com/google-apps/calendar/caldav/v2/guide#creating_your_client_id

Put the client ID and secret into `org-caldav-oauth2-client-id` and
`org-caldav-oauth2-client-secret`, respectively. Then set `org-caldav-url`
to the symbol 'google, and look up the `org-caldav-calendar-id` as
described above.

On first connection, the oauth2 library should redirect you to the
Google OAuth2 authentication site. This requires a javascript enabled
browser, so make sure that `browse-url-browser-function` is set to
something like `browse-url-firefox` (the internal shr or w3m browser
will **not** work). After authentication, you will be given a key that
you have to paste into the Emacs prompt. The oauth2 library will save
this key in Emacs' secure plist store, which is encrypted with
GnuPG. If you have not yet used a secure plist store, you will be
asked for its encryption passphrase. In the future, you should only
need to enter that passphrase again to connect with Google Calendar.

By default, plstore will **not** cache your entered password, so it
will possibly ask you **many** times. To activate caching, use

    (setq plstore-cache-passphrase-for-symmetric-encryption t)

### DETAILS

Compared to earlier versions of this package from 2012, it now does
proper two-way syncing, that means it does not matter where and how
you change an entry. You can also move Org entries freely from one
file to another, as long as they are all listed in
`org-caldav-files`. The org-icalendar package will put a unique ID on
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
complicated). The variable `org-icalendar-include-body`
denotes how many characters from the body should be included as
DESCRIPTION (by default all characters are included).

* iCalendar to Org

If you create a new iCalendar entry in your calendar, you'll get an
Org entry with SUMMARY as heading, DESCRIPTION as body and the
timestamp. However, if you *change* an existing entry in the calendar,
things get more complicated and the variable
`org-caldav-sync-changes-to-org` comes into play. Its default is the
symbol "title-and-timestamp", which means that only the entry's
heading is synced (with SUMMARY) and the timestamp gets updated, but
*not* the entry's body with DESCRIPTION.  The simple reason is that
you might loose data, since DESCRIPTION is rather limited in what it
can store. Still, you can set the variable to the symbol "all", which
will completely *replace* an existing Org entry with the entry that
gets generated from the calendar's event. You can also limit syncing
to heading and/or timestamp only.

To be extra safe, org-caldav will by default backup entries it
changes. See the variable `org-caldav-backup-file` for details.

* Org sexp entries

A special case are sexp entries like

    %%(diary-anniversary  2 2 1969) Foo's birthday
    
    * Regular meeting
      <%%(diary-float t 4 2)>

As you can see, they can appear in two different ways: plain by
themselves, or inside an Org entry. If they are inside an Org entry,
there's a good chance they will be exported (see below) and have an ID
property, so they can be found by org-caldav. We can sync the title,
but syncing the timestamp with the s-expression is just infeasible, so
this will generate a sync error (which are *not* critical; you'll just
see them at the end of the sync, just so that you're aware that some
stuff wasn't synced properly).

However, sexp-entries are insanely flexible, and there are limits as
to what the icalendar exporter will handle. For example, this here

    ** Regular event
       <%%(memq (calendar-day-of-week date) '(1 3 5))>

will not be exported at all.

If the sexp entry is not inside an Org entry but stands by itself,
they still will be exported, but they won't get an ID (since IDs are
properties linked to Org entries). In practice, that means that you
can delete and change them inside Org and this will be synced, but if
you *change* them in the *calendar*, this will *not* get synced
back. Org-caldav just cannot find those entires, so this will generate
a one-time sync error instead (again: those are not critical, just
FIY). If you don't want those entries to be exported at all, just set
`org-icalendar-include-sexps` to nil.

#### Filtering entries

There are several possibilities to choose which entries should be
synced and which not:

* If you only want to sync manually marked entries, use
  `org-caldav-select-tags`, which is directly mapped to
  `org-export-select-tags`, so see its doc-string on how it works.

* If you want to exclude certain tags, use `org-caldav-exclude-tags`,
  which is mapped to `org-icalendar-exclude` tags.

* If you want more fine grained control, use
  `org-caldav-skip-conditions`. The syntax of the conditions is
  described in the doc-string of `org-agenda-skip-if`.

Note however that the normal `org-agenda-skip-function(-global)` will
**not** have any effect on the icalendar exporter (this used to be the
case, but changed with the new exporters).

#### Syncing deletions

If you delete entries in your Org files, the corresponding iCalendar
entries will by default get deleted. You can change that behavior with
`org-caldav-delete-calendar-entries` to never delete, or to ask before
deletion.

You must be careful to not simply remove previously synced files from
`org-caldav-files`, as org-caldav would view all the entries from those
files as deleted and hence by default also delete them from the
calendar.  However, org-caldav should be able to detect this situation
and warn you with the message 'Previously synced file(s) are missing',
asking you whether to continue nonetheless.

If you delete events in your calendar, you will by default get asked
if you'd like to delete the corresponding Org event. You can change
that behavior through `org-caldav-delete-org-entries`.

If you answer a deletion request with "no", the event should get
re-synced to the calendar next time you call `org-caldav-sync`.

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

The current sync state is stored in a file `org-caldav-SOMEID.el` in
the ~/.emacs.d directory. You can change the location through the
variable `org-caldav-save-directory`. SOMEID directly depends on the
calendar id (it's a snipped MD5).

If you sync your Org files across different machines and want to use
org-caldav on all of them, don't forget to sync the org sync state,
too. Probably your best bet is to set `org-caldav-save-directory` to the
path you have your Org files in, so that it gets copied alongside with
them.

#### Starting from scratch

If your sync state somehow gets broken, you can make a clean slate by
doing

    C-u M-x org-caldav-delete-everything

The function has to be called with a prefix so that you don't call it
by accident. This will delete everything in the calendar along with
the current sync state. You can then call `org-caldav-sync` afterwards
and it will completely put all Org events into the now empty
calendar. Needless to say, don't do that if you have new events in
your calendar which are not synced yet...

Deleting many events can be slow, though; in that case, just delete
the calendar and re-create it, delete the sync state file in
~/.emacs.d and restart Emacs.

#### Syncing with more than one calendar

This can be done by setting the variable `org-caldav-calendars`. It
should be a list of plists (a 'plist' is simply a list with alternating
:key's and values). Through these plists, you can override the global
values of variables like `org-caldav-calendar-id`, and calling
`org-caldav-sync` will go through these plists in order.

Example:

    (setq org-caldav-calendars
      '((:calendar-id "work@whatever" :files ("~/org/work.org")
         :inbox "~/org/fromwork.org")
        (:calendar-id "stuff@mystuff"
         :files ("~/org/sports.org" "~/org/play.org")
         :skip-conditions (regexp "soccer")
         :inbox "~/org/fromstuff.org")) )

This means that you have two calendars with IDs "work@whatever" and
"stuff@mystuff". Both will be accessed through the global value of
org-caldav-url, since the key :url isn't specified. The calendar
"work@whatever" will be synced with the file 'work.org' and inbox
'fromwork.org', while "stuff@mystuff" with 'sports.org' and
'play.org', *unless* there's the string 'soccer' in the heading, and
and inbox is 'fromstuff.org'. See the doc-string of
`org-caldav-calendars` for more details on which keys you can use.

#### Additional stuff

See the doc-string of `org-caldav-inbox` if you want more flexibility in
where new items should be put. Instead of simply providing a file, you
can also choose an existing entry or headline.

#### Timezone problems

Timezone handling is plain horrible, and it seems every CalDAV server
does it slightly differently, also using non-standard headers like
X-WR-TIMEZONE. If you see items being shifted by a few hours, make
really really sure you have properly set `org-icalendar-timezone`, and
that your calendar is configured to use the same one.

If it still does not work, you can try setting `org-icalendar-timezone`
to the string "UTC". This will put all events using UTC times and the
server should transpose the time to the timezone you have set in your
calendar preferences. For some servers (like SOGo) this might work
better than setting a "real" timezone.

#### Troubleshooting

If org-caldav reports a problem with the given URL, please
triple-check that the URL is correct. It must point to a valid
calendar on your CalDAV server.

If the error is that the URL does not seem to accept DAV requests, you
can additionally check with 'curl' by doing

     curl -D - -X OPTIONS --basic -u mylogin:mypassword URL

The output of this command must contain a 'DAV' header like this:

    DAV: 1, 3, extended-mkcol, access-control, ... etc. ...

By default, org-caldav will put all kinds of debug output into the
buffer \*org-caldav-debug\*. Look there if you're getting sync errors
or if something plain doesn't work. If you're using an authinfo file
and authentication doesn't work, set auth-info-debug to t and look in
the \*Messages\* buffer. When you report a bug, please try to post the
relevant portion of the \*org-caldav-debug\* buffer since it might be
helpful to see what's going wrong. If Emacs throws an error, do

    M-x toggle-debug-on-error

and try to replicate the error to get a backtrace.

You can also turn on excessive debugging by setting the variable
`org-caldav-debug-level` to 2. This will also output the *contents* of
the events into the debug buffer. If you send such a buffer in a bug
report, please make very sure you have removed personal information
from those events.

#### Known Bugs

* Recurring events created or changed on the calendar side cannot be
  synced (they will work fine as long as you manage them in Org,
  though).

* Syncing is currently pretty slow since everything is done
  synchronously.

* Pretty much everything besides SUMMARY, DESCRIPTION and time is
  ignored in iCalendar.

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
