

# md2f = '~/data/quake/barrel.md2';
# md2 = read.quake.md2(ms2f);
read.quake.md2 <- function(filepath, anim = FALSE) {
  fh = file(filepath, "rb");
  on.exit({ close(fh) });

  endian = 'little';

  header = list();

  header$ident = readBin(fh, integer(), n = 1, size = 4, endian = endian);
  header$version = readBin(fh, integer(), n = 1, size = 4, endian = endian);

  if(header$ident != 844121161 | header$version != 8L) {
    stop(sprintf("File '%s' not in MD2 format.\n", filepath));
  }

  hdr_data = readBin(fh, integer(), n = 15, size = 4, endian = endian);
  header$skinwidth = hdr_data[1];
  header$skinheight = hdr_data[2];
  header$framesize = hdr_data[3];
  header$num_skins = hdr_data[4];
  header$num_vertices = hdr_data[5];
  header$num_st = hdr_data[6];
  header$num_tris = hdr_data[7];
  header$num_glcmds = hdr_data[8];
  header$num_frames = hdr_data[9];
  header$offset_skins = hdr_data[10];
  header$offset_st = hdr_data[11];
  header$offset_tris = hdr_data[12];
  header$offset_frames = hdr_data[13];
  header$offset_glcmds = hdr_data[14];
  header$offset_end = hdr_data[15];

}
