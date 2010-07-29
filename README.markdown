# simplenote.el

This is a very simple, and at the moment very rough, Emacs lisp package that can assist your interaction with the [Simplenote][sn] iPhone application and web application developed by the [Cloud Factory][cf]. Notice that `simplenote.el` is not developed or endorsed by the Cloud Factory.

This package has been tested only under Emacs 23.1.1. Although I hope it works under older versions of Emacs (at least Emacs 22) I have not tested it. Furthermore, this is pre-alpha quality software and there is a high probability that it will cause irreparable harm to your notes. Use at your own peril and keep a backup of your notes.

The software is distributed under the GPL license version 2.

## Requirements

This package relies on your system having a program that can establish secure connections to the Simplenote server, such as `openssl` or `gnutls-cli`.

### Mac OS X

`openssl` is pre-installed under Mac OS X.

### Windows

Under Windows you need to use `gnutls-cli` under Cygwin (tested with Cygwin 1.7.5-1). The Cygwin `bin` directory, by default `c:/cygwin/bin`, should be in your path. ‘Standalone´ versions of `openssl` or `gnutls-cli` do not appear to work under Windows.

## Installation and configuration

After downloading the package, put the file `simplenote.el` in a directory where Emacs can find it during startup and then add in your `.emacs` the lines

    (require 'simplenote)
    (setq simplenote-email "email@company.com")
    (setq simplenote-password "yourpassword")
    (simplenote-setup)
    
where the email and password are the ones that you use to login to the Simplenote application. It is also possible to set `simplenote-password` to `nil`. In this case you will be asked for the password when it is required.`simplenote.el` keeps a local copy of your notes in a directory defined in the variable `simplenote-directory`. Its default value is `~/.simplenote`. You can change the value of `simplenote-directory` before calling `simplenote-setup`. The latter checks for the existence of `simplenote-directory` and some necessary sub-directories and creates these if they do not exist.

## Usage

There are two usage modes for `simplenote.el`. It can be used as a browser for your notes (with local caching) or you can synchronize individual notes to the Simplenote application and later edit them on the iPhone. These two approaches do not exclude one another. I will explain these two approaches below.

### Simplenote browser

Give the command `M-x simplenote-browse`. If this the first time you will see an almost empty buffer. Click on the `[Sync]` button to download your notes from the Simplenote server. If you have not set the variables `simplenote-email` or `simplenote-password` you will be asked interactively for their values.

The rest of the interface should be more or less clear. You have the option to create new notes, and edit or delete existing notes. Syncing is not automatic but has to be invoked manually either by clicking the `[Sync]` button at the top of the Simplenote browser or by `M-x simplenote-sync-notes`.

Deleting a note just marks the note as deleted in order to give you the opportunity to undelete it if you change your mind. After syncing the note gets deleted from the local cache but still exists (marked as deleted) on the Simplenote server. The note will be permanently deleted only after syncing through the iPhone Simplenote application or by using the Simplenote API.

### Sync individual files

The idea here is that you have a text file that you want to edit on your iPhone when you can't use your laptop, for example, during a bus commute. First, while visiting the text file that you want to sync run `M-x simplenote-create-note-from-buffer`. This will create a new note on the Simplenote server with the contents of your file. Each note is identified by a **key** which is saved by `simplenote.el` in the buffer-local variable `simplenote-key`. In order for the key to persist after you close the file you must save it. For this reason the function `simplenote-create-note-from-buffer` adds a file-local variables section at the end of your file. If your file already has file-local variables you will need to do some editing.

Then you use the functions `M-x simplenote-push-buffer` and `M-x simplenote-pull-buffer` to send local changes to the server and get changes from the server respectively.

## Future

* Handle gracefully the case where there is no connection to the Simplenote server.

[cf]: http://cloud-factory.com/
[sn]: http://simplenoteapp.com/
