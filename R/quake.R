

# md2f = '~/data/quake/barrel.md2';
# md2 = read.quake.md2(md2f);
#' @title Read Quake II model in MD2 format.
#'
#' @param filepath character string, the path to the MD2 file
#'
#' @param anim logical, whether to load the whole animation (if present). Returns a list of models, the animation frames. If FALSE, only the first frame is returned.
#'
#' @note Ignore this function, it will be moved to a different package.
#'
#' @export
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

  # read model data: textures aka skins
  seek(fh, where = header$offset_skins, origin = "start");
  header$skins = list();
  if(header$num_skins > 0L) {
    for(i in 1:header$num_skins) {
      header$skins[[i]] = readBin(fh, character());
      print(header$skins[[i]])
    }
  }

  # read model data: texture coords
  seek(fh, where = header$offset_st, origin = "start");
  header$texcoords = list();
  if(header$num_st > 0L) {
    header$texcoords$s= rep(NA, (header$num_st));
    header$texcoords$t= rep(NA, (header$num_st));
    for(i in 1:header$num_st) {
      header$texcoords$s[[i]] = readBin(fh, integer(), n = 1L, size = 2L);
      header$texcoords$t[[i]] = readBin(fh, integer(), n = 1L, size = 2L);
    }
  }

  # read model data: triangles (vertex and texture indices)
  seek(fh, where = header$offset_tris, origin = "start");
  header$triangles = list();
  if(header$num_tris > 0L) {
    header$triangles$vertex = matrix(rep(NA, (header$num_tris * 3L)), ncol = 3); # vertex indices
    header$triangles$st = matrix(rep(NA, (header$num_tris * 3L)), ncol = 3); # texture coord indices
    for(i in 1:header$num_tris) {
      header$triangles$vertex[i,] = readBin(fh, integer(), n = 3L, size = 2L, signed = FALSE);
      header$triangles$st[i,] = readBin(fh, integer(), n = 3L, size = 2L, signed = FALSE);
    }
  }

  # read model data: openGL commands
  seek(fh, where = header$offset_glcmds, origin = "start");
  if(header$num_glcmds > 0L) {
      header$glcmds[i] = readBin(fh, integer(), n = header$num_glcmds, size = 4L);
  }


  ## inside frame loop:
  seek(fh, where = header$offset_frames, origin = "start");

  header$frames = list();
  if(header$num_frames > 0L) {
    pdn = predefined.md2.normals();
    for(i in 1:header$num_frames) {
      this_frame = list();
      # read model data: vertex coords (and normal vector indices into pre-defined normal vector)
      this_frame$scale = readBin(fh, numeric(), n = 3L, size = 4L);
      this_frame$translate = readBin(fh, numeric(), n = 3L, size = 4L);
      this_frame$name = readBin(fh, character());
      if(header$num_vertices > 0L) {
        this_frame$vertex_coords = matrix(rep(NA, (3 * header$num_vertices), ncol = 3L));
        this_frame$vertex_normals = matrix(rep(NA, (3 * header$num_vertices), ncol = 3L));
        for(j in 1:header$num_vertices) {
          this_vert_coords_raw = readBin(fh, integer(), n = 3L, size = 1L, signed = FALSE);
          this_vert_normal_index = readBin(fh, integer(), n = 1L, size = 1L, signed = FALSE);

          # compute real vertex coords using frame scale and translation
          this_frame$vertex_coords[j,] = (this_frame$scale * this_vert_coords_raw) + this_frame$translate;
          this_frame$vertex_normals[j,] = pdn[this_vert_normal_index,];
        }
      }
    }
  }

  return(list('header'=header));
}


#' @title Predefined MD2 normals.
#'
#' @return 3xn matrix of normals
#'
#' @keywords internal
predefined.md2.normals <- function() {
  normals_raw = c(
    -0.525731,  0.000000,  0.850651 ,
    -0.442863,  0.238856,  0.864188 ,
    -0.295242,  0.000000,  0.955423 ,
    -0.309017,  0.500000,  0.809017 ,
    -0.162460,  0.262866,  0.951056 ,
    0.000000,  0.000000,  1.000000 ,
    0.000000,  0.850651,  0.525731 ,
    -0.147621,  0.716567,  0.681718 ,
    0.147621,  0.716567,  0.681718 ,
    0.000000,  0.525731,  0.850651 ,
    0.309017,  0.500000,  0.809017 ,
    0.525731,  0.000000,  0.850651 ,
    0.295242,  0.000000,  0.955423 ,
    0.442863,  0.238856,  0.864188 ,
    0.162460,  0.262866,  0.951056 ,
    -0.681718,  0.147621,  0.716567 ,
    -0.809017,  0.309017,  0.500000 ,
    -0.587785,  0.425325,  0.688191 ,
    -0.850651,  0.525731,  0.000000 ,
    -0.864188,  0.442863,  0.238856 ,
    -0.716567,  0.681718,  0.147621 ,
    -0.688191,  0.587785,  0.425325 ,
    -0.500000,  0.809017,  0.309017 ,
    -0.238856,  0.864188,  0.442863 ,
    -0.425325,  0.688191,  0.587785 ,
    -0.716567,  0.681718, -0.147621 ,
    -0.500000,  0.809017, -0.309017 ,
    -0.525731,  0.850651,  0.000000 ,
    0.000000,  0.850651, -0.525731 ,
    -0.238856,  0.864188, -0.442863 ,
    0.000000,  0.955423, -0.295242 ,
    -0.262866,  0.951056, -0.162460 ,
    0.000000,  1.000000,  0.000000 ,
    0.000000,  0.955423,  0.295242 ,
    -0.262866,  0.951056,  0.162460 ,
    0.238856,  0.864188,  0.442863 ,
    0.262866,  0.951056,  0.162460 ,
    0.500000,  0.809017,  0.309017 ,
    0.238856,  0.864188, -0.442863 ,
    0.262866,  0.951056, -0.162460 ,
    0.500000,  0.809017, -0.309017 ,
    0.850651,  0.525731,  0.000000 ,
    0.716567,  0.681718,  0.147621 ,
    0.716567,  0.681718, -0.147621 ,
    0.525731,  0.850651,  0.000000 ,
    0.425325,  0.688191,  0.587785 ,
    0.864188,  0.442863,  0.238856 ,
    0.688191,  0.587785,  0.425325 ,
    0.809017,  0.309017,  0.500000 ,
    0.681718,  0.147621,  0.716567 ,
    0.587785,  0.425325,  0.688191 ,
    0.955423,  0.295242,  0.000000 ,
    1.000000,  0.000000,  0.000000 ,
    0.951056,  0.162460,  0.262866 ,
    0.850651, -0.525731,  0.000000 ,
    0.955423, -0.295242,  0.000000 ,
    0.864188, -0.442863,  0.238856 ,
    0.951056, -0.162460,  0.262866 ,
    0.809017, -0.309017,  0.500000 ,
    0.681718, -0.147621,  0.716567 ,
    0.850651,  0.000000,  0.525731 ,
    0.864188,  0.442863, -0.238856 ,
    0.809017,  0.309017, -0.500000 ,
    0.951056,  0.162460, -0.262866 ,
    0.525731,  0.000000, -0.850651 ,
    0.681718,  0.147621, -0.716567 ,
    0.681718, -0.147621, -0.716567 ,
    0.850651,  0.000000, -0.525731 ,
    0.809017, -0.309017, -0.500000 ,
    0.864188, -0.442863, -0.238856 ,
    0.951056, -0.162460, -0.262866 ,
    0.147621,  0.716567, -0.681718 ,
    0.309017,  0.500000, -0.809017 ,
    0.425325,  0.688191, -0.587785 ,
    0.442863,  0.238856, -0.864188 ,
    0.587785,  0.425325, -0.688191 ,
    0.688191,  0.587785, -0.425325 ,
    -0.147621,  0.716567, -0.681718 ,
    -0.309017,  0.500000, -0.809017 ,
    0.000000,  0.525731, -0.850651 ,
    -0.525731,  0.000000, -0.850651 ,
    -0.442863,  0.238856, -0.864188 ,
    -0.295242,  0.000000, -0.955423 ,
    -0.162460,  0.262866, -0.951056 ,
    0.000000,  0.000000, -1.000000 ,
    0.295242,  0.000000, -0.955423 ,
    0.162460,  0.262866, -0.951056 ,
    -0.442863, -0.238856, -0.864188 ,
    -0.309017, -0.500000, -0.809017 ,
    -0.162460, -0.262866, -0.951056 ,
    0.000000, -0.850651, -0.525731 ,
    -0.147621, -0.716567, -0.681718 ,
    0.147621, -0.716567, -0.681718 ,
    0.000000, -0.525731, -0.850651 ,
    0.309017, -0.500000, -0.809017 ,
    0.442863, -0.238856, -0.864188 ,
    0.162460, -0.262866, -0.951056 ,
    0.238856, -0.864188, -0.442863 ,
    0.500000, -0.809017, -0.309017 ,
    0.425325, -0.688191, -0.587785 ,
    0.716567, -0.681718, -0.147621 ,
    0.688191, -0.587785, -0.425325 ,
    0.587785, -0.425325, -0.688191 ,
    0.000000, -0.955423, -0.295242 ,
    0.000000, -1.000000,  0.000000 ,
    0.262866, -0.951056, -0.162460 ,
    0.000000, -0.850651,  0.525731 ,
    0.000000, -0.955423,  0.295242 ,
    0.238856, -0.864188,  0.442863 ,
    0.262866, -0.951056,  0.162460 ,
    0.500000, -0.809017,  0.309017 ,
    0.716567, -0.681718,  0.147621 ,
    0.525731, -0.850651,  0.000000 ,
    -0.238856, -0.864188, -0.442863 ,
    -0.500000, -0.809017, -0.309017 ,
    -0.262866, -0.951056, -0.162460 ,
    -0.850651, -0.525731,  0.000000 ,
    -0.716567, -0.681718, -0.147621 ,
    -0.716567, -0.681718,  0.147621 ,
    -0.525731, -0.850651,  0.000000 ,
    -0.500000, -0.809017,  0.309017 ,
    -0.238856, -0.864188,  0.442863 ,
    -0.262866, -0.951056,  0.162460 ,
    -0.864188, -0.442863,  0.238856 ,
    -0.809017, -0.309017,  0.500000 ,
    -0.688191, -0.587785,  0.425325 ,
    -0.681718, -0.147621,  0.716567 ,
    -0.442863, -0.238856,  0.864188 ,
    -0.587785, -0.425325,  0.688191 ,
    -0.309017, -0.500000,  0.809017 ,
    -0.147621, -0.716567,  0.681718 ,
    -0.425325, -0.688191,  0.587785 ,
    -0.162460, -0.262866,  0.951056 ,
    0.442863, -0.238856,  0.864188 ,
    0.162460, -0.262866,  0.951056 ,
    0.309017, -0.500000,  0.809017 ,
    0.147621, -0.716567,  0.681718 ,
    0.000000, -0.525731,  0.850651 ,
    0.425325, -0.688191,  0.587785 ,
    0.587785, -0.425325,  0.688191 ,
    0.688191, -0.587785,  0.425325 ,
    -0.955423,  0.295242,  0.000000 ,
    -0.951056,  0.162460,  0.262866 ,
    -1.000000,  0.000000,  0.000000 ,
    -0.850651,  0.000000,  0.525731 ,
    -0.955423, -0.295242,  0.000000 ,
    -0.951056, -0.162460,  0.262866 ,
    -0.864188,  0.442863, -0.238856 ,
    -0.951056,  0.162460, -0.262866 ,
    -0.809017,  0.309017, -0.500000 ,
    -0.864188, -0.442863, -0.238856 ,
    -0.951056, -0.162460, -0.262866 ,
    -0.809017, -0.309017, -0.500000 ,
    -0.681718,  0.147621, -0.716567 ,
    -0.681718, -0.147621, -0.716567 ,
    -0.850651,  0.000000, -0.525731 ,
    -0.688191,  0.587785, -0.425325 ,
    -0.587785,  0.425325, -0.688191 ,
    -0.425325,  0.688191, -0.587785 ,
    -0.425325, -0.688191, -0.587785 ,
    -0.587785, -0.425325, -0.688191 ,
    -0.688191, -0.587785, -0.425325
  );
  return(matrix(normals_raw, ncol = 3L, byrow = TRUE));
}
