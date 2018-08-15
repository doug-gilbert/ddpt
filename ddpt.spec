%define	name	ddpt
%define	version	0.96
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
ddpt is yet another variant of the Unix dd command used to copy files.
This variant is specialized for moving data to, from or between storage
devices. If requested, SCSI commands can be sent to read or write data.
SCSI commands are sent via a pass-through interface. Also supports
two variants of SCSI copy offload: xcopy(LID1) and the disk->disk
subset of xcopy(LID4) known as ODX. There are two helper utilities:
ddptctl (for ODX type copy offload) and ddpt_sgl (for scatter gather
lists)

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
* Wed Aug 15 2018 - dgilbert at interlog dot com
- see ChangeLog
  * ddpt-0.96
* Fri Dec 26 2014 - dgilbert at interlog dot com
- show percent completed, job file
  * ddpt-0.95
* Mon Apr 07 2014 - dgilbert at interlog dot com
- add ODX (subset xcopy(LID4)) support
  * ddpt-0.94
* Wed Nov 13 2013 - dgilbert at interlog dot com
- allow tape device in non-pt mode; rework signal handling, xcopy
  * ddpt-0.93
* Thu Feb 17 2011 - dgilbert at interlog dot com
- warn about pt on block partitions, coe on reg,blk in
  * ddpt-0.92
* Fri Aug 13 2010 - dgilbert at interlog dot com
- extend bpt=BPT to bpt=BPT[,OBPC], resume and trim flags
  * ddpt-0.91
* Sat May 08 2010 - dgilbert at interlog dot com
- initial version
  * ddpt-0.90


