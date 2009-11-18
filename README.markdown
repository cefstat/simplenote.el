# simplenote.el

This is a very simple, and at the moment very rough, Emacs lisp package that can
assist your interaction with the [Simplenote][sn] application created by the
[Cloud Factory][cf]. `simplenote.el` is not written or endorsed by Cloud
Factory.

`simplenote.el` has been tested only under Emacs 23.1.1. Although I hope it
works under older versions of Emacs (at least Emacs 22) I have not tested
it. Furthermore, this is pre-alpha quality software and there is a high
probability that it will cause irreparable harm to your notes. Use at your own
peril and keep a backup of your notes.

## Installation

Put the file `simplenote.el` in a directory where Emacs can find it during
startup and then in your `.emacs` add

    (require 'simplenote)
    (setq simplenote-email "email@company.com")
    (setq simplenote-password "yourpassword")
    
where the email and password are the ones that you use to login to the
Simplenote application. Your notes will be cached

## Usage

There are two usage modes for `simplenote.el`. You can use it either as a
browser for your notes (with local caching) or in order to synchronize selected
files with the Simplenote application. I will explain these two modes below.

### Simplenote browser

Give the command `M-x simplenote-browse`. If this the first time you will see an
almost empty buffer; just click on the `[Sync]` button. The interface should be
more or less clear. You have the option to create new notes, or delete existing
notes. Syncing is not automatic but has to be invoked manually either by
clicking the `[Sync]` button at the top of the Simplenote browser or by `M-x
simplenote-sync-notes`.

Deleting a note actually just marks the note as deleted in order to give you the
opportunity to undelete it if you change your mind. After syncing the note gets
deleted from the local cache but still exists (marked as deleted) at the
Simplenote website. The note will be permanently deleted only after syncing
through the iPhone Simplenote application or by using the Simplenote API.

### Sync selected files

The idea here is that you have a text file that you want to edit on your iPhone
during a bus commute. First, while visiting the text file that you want to sync
run `M-x simplenote-create-note-from-buffer`. This will create a new note on the
Simplenote server with the contents of your file. Each note is identified by a
**key** which is saved by `simplenote.el` in the buffer-local variable
`simplenote-key`. In order for the key to persist after you close the file you
must save it. For this reason the function `simplenote-create-note-from-buffer`
adds a file-local variables section at the end of your file, saves the file, and
sends its contents again to the Simplenote server. If your file already has
file-local variables you will need to do some editing.

Then you use the functions `M-x simplenote-push-buffer` and `M-x
simplenote-pull-buffer` to send local changes to the server and get changes from
the server respectively.


[cf]: http://cloud-factory.com/
[sn]: http://simplenoteapp.com/