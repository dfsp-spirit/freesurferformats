# Genrate TCK and TSF from open data --------------------------------------
# internal
# A not exported /dev might be a more appropriate location.
# However /web is already there and registered in >.Rbuildignore<, hence used.
stop('This script is for documentation only.\nDo not run automatically.\nFiles should already exist in tests/testdata/dwi.')

dest = file.path('tests', 'testdata', 'dwi') # openneuro/ds001226/sub-CON01
if (!file.exists(dest)) {
  dir.create(dest, recursive=TRUE, showWarnings=FALSE)
  # haven't used accurate API integration for this simple task
  urls = c('https://s3.amazonaws.com/openneuro.org/ds001226/sub-CON01/ses-preop/dwi/sub-CON01_ses-preop_acq-AP_dwi.bval',
           'https://s3.amazonaws.com/openneuro.org/ds001226/sub-CON01/ses-preop/dwi/sub-CON01_ses-preop_acq-AP_dwi.bvec',
           'https://s3.amazonaws.com/openneuro.org/ds001226/sub-CON01/ses-preop/dwi/sub-CON01_ses-preop_acq-AP_dwi.nii.gz')
  download.file(urls, file.path(dest, basename(urls)))
  stopifnot(basename(urls) %in% basename(list.files(dest)))

  `!=0` = function(x, y=0) x != y

  c('mrconvert', 'dwi2mask', 'dwi2response', 'dwi2fod',
    'tckgen', 'dwi2tensor', 'tensor2metric', 'tcksample') |>
    Sys.which() |>
    nchar() |>
    `!=0`() |>
    all() |>
    stopifnot()

  local({
    ap_nii  = file.path(dest, 'sub-CON01_ses-preop_acq-AP_dwi.nii.gz')
    ap_bvec = file.path(dest, 'sub-CON01_ses-preop_acq-AP_dwi.bvec')
    ap_bval = file.path(dest, 'sub-CON01_ses-preop_acq-AP_dwi.bval')

    ap_mif  = file.path(dest, 'dwi_AP.mif')
    wm_mask = file.path(dest, 'wm_mask.mif')
    response_txt = file.path(dest, 'response.txt')
    fod_file = file.path(dest, 'fod.mif')
    dt_file = file.path(dest, 'dt.mif')
    fa_file = file.path(dest, 'fa.mif')

    tck_file = file.path(dest, 'tracks.tck')
    tsf_file = file.path(dest, 'tracks.tsf')

    cmd = sprintf('
    mrconvert %s %s -fslgrad %s %s &&
    dwi2mask %s %s &&
    dwi2response tournier %s %s &&
    dwi2fod csd %s %s %s &&
    tckgen %s %s -seed_image %s -select 100000 &&
    dwi2tensor %s %s &&
    tensor2metric %s -fa %s &&
    tcksample %s %s %s',
                  ap_nii, ap_mif, ap_bvec, ap_bval,
                  ap_mif, wm_mask,
                  ap_mif, response_txt,
                  ap_mif, response_txt, fod_file,
                  fod_file, tck_file, wm_mask,
                  ap_mif, dt_file,
                  dt_file, fa_file,
                  tck_file, fa_file, tsf_file
    )
    system(cmd)
  })

  # remove temporary files
  f = list.files(dest, full.names=TRUE)
  tmp = f[!grepl('tracks\\.(tsf$|tck)$', f)]
  if (length(tmp))
    file.remove(tmp) |> invisible()

} else sprintf('%s Check if data already exists:', dest)




