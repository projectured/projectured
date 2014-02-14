#!/bin/sh

ffmpeg -y -video_size 1280x720 -f x11grab -r 30 -i :0.0+0,26 -vcodec libx264 $*

#ffmpeg -y -f alsa -ac 2 -i pulse -framerate 30 -video_size 1280x720 -f x11grab -r 30 -i :0.0+0,26 -acodec pcm_s16le -vcodec libx264 /tmp/record.mkv
