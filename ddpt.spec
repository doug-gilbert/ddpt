%define	name	ddpt
%define	version	0.91
%define	release	1

Summary:	Copy files (like dd) especially SCSI/storage devices
Name:		%{name}
Version:	%{version}
Release:	%{release}
License:	FreeBSD
Group:		Utilities/System
URL:		http://sg.danny.cz/sg/ddpt.html
Source0:	http://sg.danny.cz/sg/p/%{name}-%{version}.tgz
BuildRoot:	%{_tmppath}/%{name}-%{version}-root
Packager:	Douglas Gilbert <dgilbert at interlog dot com>

%description
ddpt is yet another variant of the Unix dd command used to copy
files. This variant is specialized for moving data to, from or
between storage devices. If requested SCSI commands can be
sent to read or write data. Such commands are sent via a pass-through
interface. Note that recent (S)ATA disks can often be driven
by SCSI commands due to SCSI to ATA translation (SAT) implemented
in the kernel.

%prep

%setup -q

%build
%configure

%install
if [ "$RPM_BUILD_ROOT" != "/" ]; then
        rm -rf $RPM_BUILD_ROOT
fi

make install \
        DESTDIR=$RPM_BUILD_ROOT

%clean
if [ "$RPM_BUILD_ROOT" != "/" ]; then
        rm -rf $RPM_BUILD_ROOT
fi


%files
%defattr(-,root,root)
%doc ChangeLog INSTALL README CREDITS AUTHORS COPYING
%attr(0755,root,root) %{_bindir}/*
# >> should that be %attr(0755,root,root) %{_sbindir}/*   ??
%{_mandir}/man8/*

%changelog
* Mon Jul 26 2010 - dgilbert at interlog dot com
- extend bpt=BPT to bpt=BPT[,OBPC], resume and trim flags
  * ddpt-0.91
* Sat May 08 2010 - dgilbert at interlog dot com
- initial version
  * ddpt-0.90
