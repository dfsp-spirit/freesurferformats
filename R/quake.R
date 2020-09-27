

# md2f = '~/data/quake/barrel.md2';
# md2 = read.quake.md2(md2f);
# sf = list('faces'=(md2$triangles$vertex + 1L), vertices=md2$frames[[1]]$vertex_coords)
# class(sf) = c(class(sf), 'fs.surface')
# fsbrain::vis.fs.surface(sf)
#
# # visualiyze with texture.
# mt = rgl::material3d()
# mt$texture = '~/data/quake/barrel.png'
# mt$specular = '#000000'
# texcoords = cbind(md2$texcoords$s, md2$texcoords$t)
# tm = tmesh3d(t(sf$vertices), t(sf$faces), homogeneous = F, material = mt, texcoords = texcoords)
# rgl::shade3d(tm)
#
# texc_t = 1.0 - (md2$texcoords$t/max(md2$texcoords$t))
# texc_s = md2$texcoords$s/max(md2$texcoords$s)
# texcoords = cbind(texc_s, texc_t)
# tm = tmesh3d(t(sf$vertices), t(sf$faces), homogeneous = F, material = mt, texcoords = texcoords)
# rgl::shade3d(tm)
#
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

  md2 = list();
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

  # read model data: skins (a.k.a. textures)
  seek(fh, where = header$offset_skins, origin = "start");
  md2$skins = list();
  if(header$num_skins > 0L) {
    for(i in 1:header$num_skins) {
      #md2$skins[[i]] = readBin(fh, character());
      md2$skins[[i]] = readChar(fh, 64L);
    }
  }

  # read model data: texture coords
  seek(fh, where = header$offset_st, origin = "start");
  md2$texcoords = list();
  md2$texcoords_unscaled = list();
  if(header$num_st > 0L) {
    md2$texcoords$s= rep(NA, (header$num_st));
    md2$texcoords$t= rep(NA, (header$num_st));
    md2$texcoords_unscaled$s= rep(NA, (header$num_st));
    md2$texcoords_unscaled$t= rep(NA, (header$num_st));
    for(i in 1:header$num_st) {
      md2$texcoords_unscaled$s[[i]] = readBin(fh, integer(), n = 1L, size = 2L) / header$skinwidth;
      md2$texcoords_unscaled$t[[i]] = readBin(fh, integer(), n = 1L, size = 2L) / header$skinheight;
    }
    md2$texcoords$s = md2$texcoords_unscaled$s / header$skinwidth;
    md2$texcoords$t = md2$texcoords_unscaled$t / header$skinheight;
  }

  # read model data: triangles (vertex and texture indices)
  seek(fh, where = header$offset_tris, origin = "start");
  md2$triangles = list();
  if(header$num_tris > 0L) {
    md2$triangles$vertex = matrix(rep(NA, (header$num_tris * 3L)), ncol = 3); # vertex indices
    md2$triangles$st = matrix(rep(NA, (header$num_tris * 3L)), ncol = 3); # texture coord indices
    for(i in 1:header$num_tris) {
      md2$triangles$vertex[i,] = readBin(fh, integer(), n = 3L, size = 2L, signed = FALSE);
      md2$triangles$st[i,] = readBin(fh, integer(), n = 3L, size = 2L, signed = FALSE);
    }
  }

  # read model data: openGL commands
  seek(fh, where = header$offset_glcmds, origin = "start");
  if(header$num_glcmds > 0L) {
      md2$glcmds = readBin(fh, integer(), n = header$num_glcmds, size = 4L);
  }


  ## inside frame loop:
  seek(fh, where = header$offset_frames, origin = "start");

  md2$frames = list();
  if(header$num_frames > 0L) {
    pdn = predefined.md2.normals();
    for(i in 1:header$num_frames) {
      this_frame = list();
      # read model data: vertex coords (and normal vector indices into pre-defined normal vector)
      this_frame$scale = readBin(fh, numeric(), n = 3L, size = 4L);
      this_frame$translate = readBin(fh, numeric(), n = 3L, size = 4L);
      #this_frame$name = readBin(fh, character());
      this_frame$name = readChar(fh, 16L);
      if(header$num_vertices > 0L) {
        this_frame$vertex_coords = matrix(rep(NA, (3 * header$num_vertices)), ncol = 3L);
        this_frame$vertex_normals = matrix(rep(NA, (3 * header$num_vertices)), ncol = 3L);
        for(j in 1:header$num_vertices) {
          this_vert_coords_raw = readBin(fh, integer(), n = 3L, size = 1L, signed = FALSE);
          this_vert_normal_index = readBin(fh, integer(), n = 1L, size = 1L, signed = FALSE);

          # compute real vertex coords using frame scale and translation
          this_frame$vertex_coords[j,] = (this_frame$scale * this_vert_coords_raw) + this_frame$translate;
          this_frame$vertex_normals[j,] = pdn[this_vert_normal_index,];
        }
      }
      md2$frames[[i]] = this_frame;
    }
  }

  expected_framesize = 12L + 12L + 16L + (4L * header$num_vertices);
  if(header$framesize != expected_framesize) {
    warning(sprintf("Framesize from header is %d, expected %d.\n", header$framesize, expected_framesize));
  }

  md2$header = header;
  class(md2) = c(class(md2), 'quakemodel_md2');
  return(md2);
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


#quadf = '~/data/q2_pak/models/items/quaddama/tris.md2'
#md2q = read.quake.md2(quadf);
#' @note This function will be moved out of this package.
#' @export
is.quakemodel_md2 <- function(x) inherits(x, 'quakemodel_md2')


#' @note This function will be moved out of this package.
#' @export
vis.quakemodel_md2 <- function(md2, texture_file = NULL, frame_idx=1L) {
  if(!is.quakemodel_md2(md2)) {
    md2 = read.quake.md2(md2);
  }
  sf = list('faces'=(md2$triangles$vertex + 1L), vertices=md2$frames[[frame_idx]]$vertex_coords);
  class(sf) = c(class(sf), 'fs.surface');

  tm = rgl::tmesh3d(t(sf$vertices), t(sf$faces), homogeneous = FALSE);

  material = rgl::material3d();


  rgl::open3d();

  if(is.null(texture_file)) {
    material$color = '#333333';
    material$specular = '#AAAAAA';
    material$fog = FALSE;
    rgl::shade3d(tm, material = material)
  } else {
    if(! file.exists(texture_file)) {
      stop(sprintf("Texture file '%s' not readable.\n", texture_file));
    }
    material$color = '#FFFFFF';
    material$texture = texture_file;
    material$specular = '#000000';

    #scale_w = md2$header$skinwidth;
    #scale_h = md2$header$skinheight;
    #scale_w = dim(readbitmap::read.bitmap(texture_file))[1];
    #scale_h = dim(readbitmap::read.bitmap(texture_file))[2];
    scale_w = 1.0;
    scale_h = 1.0;

    texcoords = cbind((md2$texcoords_unscaled$s / scale_w), (1.0 - (md2$texcoords_unscaled$t / scale_h)));
    tm = rgl::tmesh3d(t(sf$vertices), t(sf$faces), homogeneous = F, material = material, texcoords = texcoords)
    rgl::shade3d(tm)
  }
}


# pcxf = '~/data/q2_pak/models/items/quaddama/skin.pcx';
#' @note This function will be moved out of this package.
#' @export
read.quake.image.pcx <- function(filepath) {
  fh = file(filepath, "rb");
  on.exit({ close(fh) });

  endian = 'little';

  pcx = list();
  header = list();

  header$ident = readBin(fh, integer(), n = 1, size = 1, endian = endian);
  if(header$ident != 10L) {
    stop("File not in PCX format.");
  }
  header$painbrush_version = readBin(fh, integer(), n = 1, size = 1, endian = endian);
  header$encoding_type = readBin(fh, integer(), n = 1, size = 1, endian = endian); # 0 = none, 1 = runlength enc.
  header$bitpix = readBin(fh, integer(), n = 1, size = 1, endian = endian); # bits per pixel, defines number of possible colors in image. 1 = 2, 2 = 4, 3 = 16, 4 = 256.

  header$minx = readBin(fh, integer(), n = 1, size = 2, endian = endian);
  header$miny = readBin(fh, integer(), n = 1, size = 2, endian = endian);
  header$maxx = readBin(fh, integer(), n = 1, size = 2, endian = endian);
  header$maxy = readBin(fh, integer(), n = 1, size = 2, endian = endian);
  header$res_horizontal = readBin(fh, integer(), n = 1, size = 2, endian = endian); # DPI
  header$res_vertical = readBin(fh, integer(), n = 1, size = 2, endian = endian); # DPI
  header$ega_palette = readBin(fh, integer(), n = 16 * 3L, size = 1, endian = endian); # the EGA palette, used for 16-color images (pitpix = 3).
  header$reserved1 = readBin(fh, integer(), n = 1, size = 1, endian = endian);
  header$num_channels = readBin(fh, integer(), n = 1, size = 1, endian = endian);
  header$bytes_per_channels_line = readBin(fh, integer(), n = 1, size = 2, endian = endian);
  header$palette_mode = readBin(fh, integer(), n = 1, size = 2, endian = endian); # 1 = color/monochrome, 2=grayscale
  header$screen_size_horizontal = readBin(fh, integer(), n = 1, size = 2, endian = endian); # horizontal screen resolution of source system
  header$screen_size_vertical = readBin(fh, integer(), n = 1, size = 2, endian = endian); # vertical
  header$reserved2 = readBin(fh, integer(), n = 54, size = 1, endian = endian);

  pcx$header = header;

  img_num_pixels = header$res_horizontal * header$res_vertical;
  img_num_values = img_num_pixels * header$num_channels;
  img_data = array(rep(NA, img_num_values), dim = c(header$res_horizontal, header$res_vertical, header$num_channels));

  # TODO: read color data here

  pcx$data = img_data;
  return(pcx);
}
