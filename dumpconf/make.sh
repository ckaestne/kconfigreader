linuxdir=../../LinuxAnalysis/master/linux
gcc dumpconf.c $linuxdir/scripts/kconfig/zconf.tab.o -I $linuxdir/scripts/kconfig/ -Wall -o dumpconf
cp dumpconf ../binary/
