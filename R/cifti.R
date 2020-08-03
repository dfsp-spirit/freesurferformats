# Functions for working with CIFTI files.
#
# We use the 'cifti' package by J. Muschelli to access data in CIFTI files whenever possible.
# Afaict, that package supports CIFTI v1 only though, so we may need to come up with something for CIFTI v2 files, which
# are not backwards compatible with v1. CIFTI v2 uses a NIFTI v2 header, and it seems 'oro.nifti' only supports readings v1 NIFTI headers,
# so we may need to come up with something there as well, but the changes from the NIFTI v1 header seem minor, mostly datatypes were
# changed to support better precision/large data ranges.
#
# General CIFTI information: https://www.nitrc.org/projects/cifti/
# Specs:
#  - CIFTI v1 spec: https://www.nitrc.org/plugins/mwiki/index.php/cifti:Cifti-1
#  - CITFI v2 full spec (PDF): https://www.nitrc.org/forum/attachment.php?attachid=341&group_id=454&forum_id=1955
# Related:
#  - CIFTI v2 changes from v1: https://www.nitrc.org/forum/forum.php?thread_id=4381&forum_id=1955
#  - Great article on NIFTI v2 format by A Winkler: https://brainder.org/2015/04/03/the-nifti-2-file-format/
