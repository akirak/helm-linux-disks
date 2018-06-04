# helm-linux-disks

[udiskie](https://github.com/coldfix/udiskie) is a useful program for mounting/unmounting removable devices in Linux, but it doesn't support LVM at present. This `helm-linux-disks` package provides an alternative, manual interface for mounting volumes in Linux through udisks2 with minimal support for LVM. 

## Features

- Display a hierarchy of block devices using `lsblk` command
- Use `udisksctl` command to run a bunch of operations on a device
  - Mount/unmount a file system
    - Kill Emacs buffers under its mount point before unmounting the file system
  - Lock/unlock a dm-crypted device
  - Power off a device
- Primitive support for LVM which helps you handle a LVM-on-LUKS device
  - Activate/deactivate a LVM volume group
- Run dired on the mount point of a device
- Run terminal on the mount point of a device
- You can operate on multiple devices without closing the Helm window through `helm-execute-persistent-action`

## Prerequisites

- Emacs 25.1
- Helm
- [udisks2](https://wiki.archlinux.org/index.php/Udisks) package and `udisksctl` command

Because udisks2 is specific to Linux, `helm-linux-disks` is for Linux.

## Installation

This package is not available on MELPA. Maybe it will never be on MELPA because it is limited to Linux? Use Quelpa or something to install the package from this repository.

To perform an actual mount/unmount operation on a device, `udisksd` daemon must be running. See [this tutorial](https://wiki.archlinux.org/index.php/Udisks) for information.

## Usage

[udiskie](https://github.com/coldfix/udiskie) is an easier, automatic way to **mount** removable devices. helm-linux-disks may be useful when you **unmount** a device.

The main entry point is `helm-linux-disks` command, which is a Helm interface displaying a list of devices.

### Operations on a lsblk entry

The primary Helm source of the command displays entries from `lsblk` command.

It supports various operations on the selected device or on the mount point of the device:

- "Dwim" action, i.e. mount/unmount, lock/unlock, or power off the entire device depending on the type and state of the entry
- Run dired
- Run a terminal
- Display information

The type of the terminal is customizable as `linux-disk-terminal-type` variable.

### Operations on a LVM volume group

The secondary Helm source of the command displays a list of LVM volume groups. The following operations on a selected volume group are supported:

- Deactivate all the logical volumes in the volume group (default)
- Activate all the logical volumes in the volume group
- Display information on the volume group

Note that there is no dwim action for a volume group. The default action deactivates all the logical volumes in the group, so use it with caution. This is because the LVM support is primarily intended for removing a device.

### Multiple operations

Because `helm-linux-disks` is based on Helm, the following commands are supported in the session:

- `helm-execute-persistent-action` (default `C-j`) runs the default action but keep the Helm window open
- `helm-refresh` (default `C-c C-u`) refresh contents in the Helm buffer

This can be especially useful in removing a device. For example, if you have to remove a storage device `/dev/sdb` containing a file system `/dev/sdb1` which is currently mounted at somewhere, you can take the following steps in `helm-linux-disks`:

1. Move the cursor to `/dev/sdb1`.
2. `C-j` to unmount the file system.
3. `C-c C-u` to refresh the list.
4. Move the cursor to `/dev/sdb`.
5. `C-j` to power off the device.
6. Close the Helm window.

Likewise, you can take a similar process to remove a crypted device:

1. Unmount the file system.
2. Lock the device.
3. Power off the device.

You can even operate on a device with an encrypted partition containing a LVM physical volume:

1. Unmount the file systems in the device.
2. Deactivate the corresponding volume group.
3. Lock the device.
4. Power off the device.

Note that the pattern of running `C-j` (`helm-execute-persistent-action`) and `C-c C-u` (`helm-refresh`) in sequence is so common when you use `helm-linux-disks`.

## Non-goals

The following goals are **not** supported by this utility:

- Create/remove/shrink/expand a file system: You can use GParted (or a command line) for those tasks. However, `helm-linux-disks` can help mount/unmount file systems.
- Mac or Windows support: They won't be supported because those platforms have neither `lsblk` nor udisks2. Because `helm-linux-disks` is basically a wrapper around `lsblk` and `udisksctl`, supporting platforms with none of those means re-implementing most parts of the package. And I suppose those platforms already have a good GUI utility for the purpose.

## Alternatives

The following volume mounters are known to exist for Linux:

- [udiskie](https://github.com/coldfix/udiskie)
- [GNOME Disks](https://en.wikipedia.org/wiki/GNOME_Disks)

## License

GPL v3
