#! /usr/bin/perl
#
# File: manage.pl
# Time-stamp: <2016-07-04 22:01:36 ska>
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

my $Version = "1.0";
######################################################################
##                             OPTIONS
######################################################################
GetOptions(
           "install|i"  => \my $install,
           "collect|c"  => \my $collect,
           "help|h!"    => \my $help,
           "longhelp!"  => \my $longhelp,
           "version|v!" => \my $version
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
  ("dot.emacs.d/init.el" => ".emacs.d/init.el",
   "dot.emacs.d/lisp/chb-util.el" => ".emacs.d/lisp/chb-util.el",
   "dot.emacs.d/lisp/active-menu.el" => ".emacs.d/lisp/chb-util.el",
   "dot.emacs.d/lisp/highlight-context-line.el" => ".emacs.d/lisp/highlight-context-line.el",
   "dot.emacs.d/lisp/linmag-mode.el" => ".emacs.d/lisp/linmag-mode.el",
   "dot.emacs.d/lisp/mtorus.el" => ".emacs.d/lisp/mtorus.el",
   "bash/dot.bash_ska" => ".bash_ska",
   "bash/dot.bash_aliases" => ".bash_aliases",
   "bash/dot.color-man" => ".color-man",
   "dot.screenrc" => ".screenrc"
  );
my $home = $ENV{HOME};

unless (defined($home)) {
    die "Can not find HOME env var.";
}
if ($collect) {
    run_collect();
} elsif ($install) {
    run_install();
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
        if ( -f $real && !$force) {
            print "Installing: \"$repo\" --> \"$real\"\n";
            copy($repo, $real) or die
              "Could not copy \"$repo\" to \"$real\": $!";
        }
    }
}
sub run_collect {
    for my $repo (keys %files) {
        my $real = "$home/$files{$repo}";
        if (compare($real, $repo) != 0) {
            print "Getting update: \"$real\" --> \"$repo\"\n";
            copy($real, $repo) or die
              "Could not copy \"$real\" to \"$repo\": $!";
        }
    }
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
