#! /usr/bin/perl
#
# File: manage.pl
# Time-stamp: <2022-08-22 11:10:57 ska>
#
# Copyright (C) 2016 by Stefan Kamphausen
#
# Author: Stefan Kamphausen
#
# Description:
# Use this file to manage the init files
use strict;
use warnings;
use Data::Dumper;
use IO::File;
use Carp;


use Getopt::Long;
use Pod::Usage;

use File::Compare;
use File::Copy;
use File::Basename qw/dirname/;
use File::Path qw/make_path/;

my $Version = "1.0";
######################################################################
##                             OPTIONS
######################################################################
GetOptions(
           "install|i"   => \my $install,
           "collect|c"   => \my $collect,
           "kdeconfig|k" => \my $kde_config,
           "dryrun|n"    => \my $dryrun,
           "force|f"     => \my $force,
           "help|h"     => \my $help,
           "longhelp"   => \my $longhelp,
           "version|v"  => \my $version
           ) or pod2usage(
                          verbose    => 0,
                          exitstatus => 1
                          );
if ($help) {
    pod2usage(
              verbose    => 1,
              exitstatus => 0
              );
}
if ($longhelp) {
    pod2usage(
              verbose    => 2,
              exitstatus => 0
              );
}
if ($version) {
    print $Version;
    exit 0;
}
######################################################################
##                               MAIN
######################################################################
# all values are relative to $HOME!
my %files =
  ("dot.emacs.d/init.el"             => ".emacs.d/init.el",
   "dot.emacs.d/lisp/chb-util.el"    => ".emacs.d/lisp/chb-util.el",
   "dot.emacs.d/lisp/active-menu.el" => ".emacs.d/lisp/active-menu.el",
   "dot.emacs.d/lisp/linmag-mode.el" => ".emacs.d/lisp/linmag-mode.el",
   "dot.emacs.d/lisp/mtorus.el"      => ".emacs.d/lisp/mtorus.el",
   "bash/dot.bash_ska"               => ".bash_ska",
   "bash/dot.bash_aliases"           => ".bash_aliases",
   "bash/dot.color-man"              => ".color-man",
   "dot.screenrc"                    => ".screenrc"
  );

my %kde_settings =
  ("kwinrc" =>
   {"Windows" => [
                  ["AutoRaise",   "false"],
                  ["FocusPolicy", "FocusFollowsMouse"],
                  ["RollOverDesktops", "false"],
                  ["GeometryTip", "true"],
                 ],
    "Desktops" => [
                   ["Name_1", "1.1"],
                   ["Name_2", "1.2"],
                   ["Name_3", "1.3"],
                   ["Name_4", "2.1"],
                   ["Name_5", "2.2"],
                   ["Name_6", "2.3"],
                   ["Name_7", "3.1"],
                   ["Name_8", "3.2"],
                   ["Name_9", "3.3"],
                   ["Number", "9"],
                   ["Rows", "3"],
                  ],
    "ElectricBorders" => [
                          ["Bottom",      "None"],
                          ["BottomLeft",  "None"],
                          ["BottomRight", "None"],
                          ["Left",        "None"],
                          ["Right",       "None"],
                          ["Top",         "None"],
                          ["TopLeft",     "None"],
                          ["TopRight",    "None"],
                         ],
    "ModifierOnlyShortcuts" => [["Meta", ""]],
    "MouseBindings" => [
                        ["CommandActiveTitlebar1", "Raise"],
                        ["CommandActiveTitlebar2", "Nothing"],
                        ["CommandActiveTitlebar3", "Operations menu"],
                        ["CommandAll1", "Move"],
                        ["CommandAll2", "Resize"],
                        ["CommandAll3", "Toggle raise and lower"],
                        ["CommandAllKey", "Meta"],
                        ["CommandAllWheel", "Nothing"],
                        ["CommandInactiveTitlebar1", "Activate and raise"],
                        ["CommandInactiveTitlebar2", "Nothing"],
                        ["CommandInactiveTitlebar3", "Operations menu"],
                        ["CommandTitlebarWheel", "Nothing"],
                        ["CommandWindow1", "Activate, raise and pass click"],
                        ["CommandWindow2", "Activate and pass click"],
                        ["CommandWindow3", "Activate and pass click"],
                        ["CommandWindowWheel", "Scroll"],
                       ],
   },
   "plasmarc" =>
   {
    "Theme" => [["name", "breeze-dark"]],
    "Wallpapers" => [["usersWallpapers", "/home/ska/etc/desktop/WS2.JPG"]]
   },
   );

my $home = $ENV{HOME};

unless (defined($home)) {
    die "Can not find HOME env var.";
}
if ($collect) {
    run_collect();
} elsif ($install) {
    run_install();
} elsif ($kde_config) {
    run_kde_config();
} else {
    pod2usage(verbose => 0, message => "No operation mode.",
             exitstatus => 11);
}
exit;

######################################################################
##                               SUBS
######################################################################
sub run_install {
    for my $repo (keys %files) {
        my $real = "$home/$files{$repo}";

        if ( -f $real && $force) {
            print "Updating: \"$repo\" --> \"$real\"\n";
        } elsif (! -f $real) {
            print "Installing: \"$repo\" --> \"$real\"\n";
        } else {
            print "Skipping \"$repo\"\n";
            next;
        }

        my $dir = dirname($real);
        if (! -d $dir) {
            print "Creating directory and parents \"$dir\"\n";
            make_path($dir) or die
              "Can't create path: $!\n";
        }
        if ($dryrun) {
            print "copy $repo -> $real\n";
        } else {
            copy($repo, $real) or die
              "Could not copy \"$repo\" to \"$real\": $!";
        }
    }
}
sub run_kde_config {
    install_kwin_extensions();
    for my $file (keys %kde_settings) {
        my %file_settings = %{ $kde_settings{$file} };
        for my $group (keys %file_settings) {
            my @grp_settings = @{ $file_settings{$group} };
            for my $setting (@grp_settings) {
                my @cmd =
                  ("kwriteconfig5",
                   "--file", $file,
                   "--group", $group,
                   "--key", $setting->[0],
                   $setting->[1]);
                if ($dryrun) {
                    print "call: " . join(" ", @cmd) . "\n";
                } else {
                    system(@cmd) == 0 or die
                      "Error in kwriteconfig5" . $! . Dumper(\@cmd);
                }
            }
        }
    }
    my $kwin_restart = "qdbus org.kde.KWin /KWin reconfigure";
    if ($dryrun) {
        print "call: $kwin_restart\n";
    } else {
        system($kwin_restart) == 0
          or die "Can't reconfigure kwin: $!\n";
    }
    # ? qdbus org.kde.klauncher /KLauncher org.kde.KLauncher.reparseConfiguration
}
sub run_collect {
    for my $repo (keys %files) {
        my $real = "$home/$files{$repo}";
        if (compare($real, $repo) != 0) {
            print "Getting update: \"$real\" --> \"$repo\"\n";
            if ($dryrun) {
                print "copy $real -> $repo\n";
            } else {
                copy($real, $repo) or die
                  "Could not copy \"$real\" to \"$repo\": $!";
            }
        }
    }
}

sub install_kwin_extensions {
    my $pack = "plasmapkg2 --type kwindscript";
    # installed already? If so, update
    # else install
}
__END__
######################################################################
##                             Now Docs...
######################################################################
=head1 NAME

  manage.pl - Manage init files in this repository

=head1 SYNOPSIS

  manage.pl [-h] [-v] [-f] [-c] [-i]

=head1 OPTIONS

=over

=item B<-i, --install>

Install files from this repo into your home directory.  Will not
overwrite already existing files unless forced with --force.

=item B<-c, --collect>

Collect files from your home back into the repo so that you can
compare and possibly update the repo.

=item B<-k, --kdeconfig>

Run KDE configuration using kwriteconfig5.

=item B<-n, --dryrun>

Don't perform any actions, just print what would happen.

=item B<-f, --force>

Force overwriting already existing files during install (aka update).

=item B<-h, --help>

Print help message and exit successfully.

=item B<--longhelp>

Print program documentation.

=item B<-v, --version>

Print version information and exit successfully.

=back

=cut
