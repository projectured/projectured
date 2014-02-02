#!/bin/sh

ffmpeg -y -video_size 1280x720 -f x11grab -r 30 -i :0.0+0,26 -vcodec libx264 $*
