#!/bin/bash

if [ $1 ]; then
    if [ $1 == "pixie" ]; then
	RND=rndr
	SHD=sdrc
	TEX=texmake
	OPT="-smode periodic -tmode periodic"
    elif [ $1 == "aqsis" ]; then
	RND=aqsis
	SHD=aqsl
	TEX=teqser
	OPT="-wrap=periodic"
    elif [ $1 == "3delight" ]; then
	RND=renderdl
	SHD=shaderdl
	TEX=tdlmake
	OPT="-mode periodic"
    fi
fi

if [ -z $RND ]; then
    echo please specify which renderer you want to use:
    echo " * 3delight"
    echo " * pixie"
    echo " * aqsis"
    exit
fi

convert_texture () {
    for var in "$@"
    do
	convert $var.png tmp.tif
	$TEX $OPT tmp.tif $var.tex
	convert $var.png -channel A -negate -separate -type TrueColor tmp.tif
	$TEX $OPT tmp.tif $var-mask.tex
	rm tmp.tif
    done
}

$SHD *.sl
convert_texture $(grep '# texture name' plant.rib | cut -f 4 -d " " | uniq)

echo Rendering shadow.rib
$RND shadow.rib
echo Rendering scene.rib
$RND scene.rib
rm *.tex *.sdr *.sdl *.slx PixieTemp* spot.depth -fr
