#!/bin/sh

WINDOW_INFO=$(xwininfo -name 'Projectional Editor')

WINDOW_SIZE=$(echo $WINDOW_INFO | grep -oEe 'geometry [0-9]+x[0-9]+' | grep -oEe '[0-9]+x[0-9]+')
WINDOW_XY=$(echo $WINDOW_INFO | grep -oEe 'Corners:\s+\+[0-9]+\+[0-9]+' | grep -oEe '[0-9]+\+[0-9]+' | sed -e 's/+/,/' )

ffmpeg -f alsa -i hw:0,0 -f x11grab -r 25 -s $WINDOW_SIZE -i :0.0+$WINDOW_XY -vcodec libx264 -preset ultrafast -crf 0 -acodec pcm_s16le -y -threads 5 output.mkv
