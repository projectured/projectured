#!/bin/sh

ffmpeg -y -video_size 1280x800 -f x11grab -r 30 -i :0.0 -vcodec libx264 $*
