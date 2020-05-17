# Notes on macOS App Packaging

I once downloaded a .app I generated where Info.plist wasn't under Contents.  When I
tried to run it I got an error like "A file can't be found".  Once I fixed the .app
and redownloaded it I kept having this error until I cleared macOS' download cache:

```
sqlite3 ~/Library/Preferences/com.apple.LaunchServices.QuarantineEventsV* 'delete from LSQuarantineEvent'
```

After that, I was able to start up the .app and approve it since it's from an unknown
developer.

https://www.macobserver.com/tips/how-to/your-mac-remembers-everything-you-download-heres-how-to-clear-download-history/

For general use, here's how to remove the quarantine attribute from a download file:

https://superuser.com/a/526949
