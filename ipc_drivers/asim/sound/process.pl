#!/s/std/bin/perl

if ($#ARGV != 0) {
    print "Supply a file. " . $#ARGV;
    exit;
}

open (MYFILE, $ARGV[0]);
while (<MYFILE>) {
    chomp;
    $line = $_;
    if ($line =~ /^EXPORT_SYMBOL.*/) {
        print "// " . $line . " // MJR\n";
        next;
    }
    
    if ($line =~ /^static (.*?)\((.*)/) {
        $full_result = "/* static MJR */ $1\($2\n";
        if ($line =~ /.*DECLARE_RWSEM.*/ ||
            $line =~ /.*LIST_HEAD.*/ ||
            $line =~ /.*DEFINE_MUTEX*/ ||
            $line =~ /.*DEFINE_SPINLOCK.*/) {
        } else {
            print $full_result;
            next;
        }
    }

    if ($line =~ /(.*)snd_printk(.*)/) {
        print "$1snd_mprintk$2 // MJR\n";
        next;
    }

    if ($line =~ /(.*)(\s)printk(.*)/) {
        print "$1$2mprintk$3 // MJR\n";
        next;
    }

    if ($line =~ /(.*)(\s)strlcpy(.*)/) {
        print "$1$2strncpy$3 // MJR\n";
        next;
    }

    if ($line =~ /^module_(init|exit)(.*)/) {
        print "// module_$1$2 // MJR\n";
        next;
    }

    if ($line =~ /(.*)(\s)__init(.*)/) {
        print "$1$2/* __init MJR */ $3\n";
        next;
    }

    print $line . "\n";
}
close (MYFILE); 
