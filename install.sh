#!/bin/bash
#
# set "THE_USER" to SUDO_USER if that's set,
#  else set it to USERNAME if THAT is set,
#   else set it to the string "unknown"
# should probably then test to see if it's "unknown"
#
THE_USER=${SUDO_USER:-${USERNAME:-unknown}}

# sudo -u ${THE_USER} normal_command_1
# root_command_1
# root_command_2
# sudo -u ${THE_USER} normal_command_2

ls
