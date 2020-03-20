% #!/usr/bin/octave -q
function dpx2map(varargin)
% Generate a surface map of data stored as DPV or DPF, using a custom
% colourscale. The result is saved as either OBJ/MTL pair or PLY, and
% can be imported for scene construction and rendering in 3D applications.
% In addition, saves also a PNG file represending the colourscale (without
% labels).
%
% Usage:
% dpx2map(dpxfile,srffile,oprefix,mapname,datarange,showrange,...
%           dual,colourgap,coption,mapsize,imgsize,templatemtl)
%
% dpxfile     : Data-per-face file, in ASCII format
% srffile     : Surface file, in ASCII format.
% oprefix     : Filename (prefix) for the output files.
% mapname     : An OCTAVE colourmap (default: 'jet').
% datarange   : Interval to be used to define the colourscale [min max].
%               If not specified, it uses the min and max of the DPX file.
% showrange   : Interval to be shown (coloured) [min max]. If not specified,
%               If not specified, it uses the same as datarange.
% dual        : True/False. If true, applies the map to the values of no
%               overlap between datarange and showrange. Useful for
%               thresholded positive+negative maps. Default is false.
% colourgap   : colour for values that off the colourscale, including NaN.
%               Default is light gray, 25%, i.e. [.75 .75 .75];
% coption     : True/False. The behavior varies if 'dual' is true or not.
%               - For 'dual' = true:
%                 If coption is false, don't rescale the extremities of the
%                 colourmap. Default is true, i.e., produce a higher contrast.
%               - For 'dual' = false:
%                 If coption is false, show the out-of-range values with the
%                 colour specified by 'colourgap'. Default is true, so the
%                 out-of-range values are shown with the extremities of the
%                 colourmap.
% mapsize     : Maximum number of colours in the colourmap. Default are:
%               - Vertexwise data:  2^16    (max allowed = 2^24)
%               - Facewise data:    2^15-1  (max allowed = 2^15-1 = 32767)
% imgsize     : Size, in [lines columns] for the colourbar PNG file.
%               Default is [200 10].
%               To prevent producing this file, use [0 0].
% templatemtl : Optional. MTL file containing a template material from
%               which the parameters will be extracted. This file
%               must contain only 1 material. Use the mtlread and mtlwrite
%               to remove unnecessary materials if needed. This applies
%               only to facewise (DPF) data.
%
% _____________________________________
% Anderson M. Winkler
% Yale University / Institute of Living
% Aug/2011
% http://brainder.org

% Do the OCTAVE stuff, using TRY to ensure MATLAB compatibility
try
    % Get the inputs
    varargin = argv();

    % Disable memory dump on SIGTERM
    sigterm_dumps_octave_core(0);

    % Print usage if no inputs are given
    if isempty(varargin) || strcmp(varargin{1},'-q'),
        fprintf('Generate a surface map of data stored as DPV or DPF, using a custom\n');
        fprintf('colourscale. The result is saved as either OBJ/MTL pair or PLY, and\n');
        fprintf('can be imported for scene construction and rendering in 3D applications.\n');
        fprintf('In addition, saves also a PNG file represending the colourscale (without\n');
        fprintf('labels).\n');
        fprintf('\n');
        fprintf('Usage:\n');
        fprintf('dpx2map <dpxfile> <srffile> <oprefix> [mapname] [datarange] [showrange]\\\n');
        fprintf('    [dual] [colourgap] [coption] [mapsize] [imgsize] [templatemtl]\n');
        fprintf('\n');
        fprintf('dpxfile     : Data-per-face file, in ASCII format\n');
        fprintf('srffile     : Surface file, in ASCII format.\n');
        fprintf('oprefix     : Filename (prefix) for the output files.\n');
        fprintf('mapname     : An OCTAVE colourmap (default: ''jet'').\n');
        fprintf('datarange*  : Interval to be used to define the colourscale [min max].\n');
        fprintf('              If not specified, it uses the min and max of the DPX file.\n');
        fprintf('showrange*  : Interval to be shown (coloured) [min max]. If not specified,\n');
        fprintf('              If not specified, it uses the same as datarange.\n');
        fprintf('dual        : True/False. If true, applies the map to the values of no\n');
        fprintf('              overlap between datarange and showrange. Useful for\n');
        fprintf('              thresholded positive+negative maps. Default is false.\n');
        fprintf('colourgap*   : colour for values that off the colourscale, including NaN.\n');
        fprintf('              Default is light gray, 25%%, i.e. [.75 .75 .75].\n');
        fprintf('coption     : True/False. The behavior varies if ''dual'' is true or not.\n');
        fprintf('              - For ''dual'' = true:\n');
        fprintf('                If coption is false, don''t rescale the extremities of the\n');
        fprintf('                colourmap. Default is true, i.e., produce a higher contrast.\n');
        fprintf('              - For ''dual'' = false:\n');
        fprintf('                If coption is false, show the out-of-range values with the\n');
        fprintf('                colour specified by ''colourgap''. Default is true, so the\n');
        fprintf('                out-of-range values are shown with the extremities of the\n');
        fprintf('                colourmap.\n');
        fprintf('mapsize     : Maximum number of colours in the colourmap. Default are:\n');
        fprintf('              - Vertexwise data:  2^16    (max allowed = 2^24)\n');
        fprintf('              - Facewise data:    2^15-1  (max allowed = 2^15-1 = 32767)\n');
        fprintf('imgsize*    : Size, in [lines columns] for the colourbar PNG file.\n');
        fprintf('              Default is [200 10].\n');
        fprintf('              To prevent producing this file, use [0 0].\n');
        fprintf('templatemtl : Optional. MTL file containing a template material from\n');
        fprintf('              which the parameters will be extracted. This file\n');
        fprintf('              must contain only 1 material. Use the mtlread and mtlwrite\n');
        fprintf('              to remove unnecessary materials if needed. This applies\n');
        fprintf('              only to facewise (DPF) data.\n');
        fprintf('* Should be entered between single quotes, e.g. ''[...]''\n');
        fprintf('\n')
        fprintf('Use placeholders '''', ''[]'' or [] for arguments not given.\n')
        fprintf('\n');
        fprintf('_____________________________________\n');
        fprintf('Anderson M. Winkler\n');
        fprintf('Yale University / Institute of Living\n');
        fprintf('Aug/2011\n');
        fprintf('http://brainder.org\n');
        
        return;
    end
end

% Defaults
d.mapname  = 'jet';          % arg 4
d.dual     = false;          % arg 7
d.colourgap = [.75 .75 .75];  % arg 8
d.coption  = true;           % arg 9
d.mapsize  = 2^16;           % arg 10
d.imgsize  = [300 20];       % arg 11

% Default material [Phong (1975) reflection model]
d.mtl.Ns    = 100;      % Specular weighting
d.mtl.Kd    = [0 0 0];  % Diffuse colour  (RGB, 0-1)
d.mtl.Ks    = [1 1 1];  % Specular colour (RGB, 0-1)
d.mtl.Ka    = [0 0 0];  % Ambient colour  (RGB, 0-1)
d.mtl.Ni    = 1;        % Refraction index
d.mtl.d     = 1;        % Dissolve factor (pseudo transparency)
d.mtl.illum = 2;        % colour on, ambient on and specular on = code 2

% Accept arguments
v = struct(            ...
    'dpxfile',    [],  ...  % arg 1
    'srffile',    [],  ...  % arg 2
    'oprefix',    [],  ...  % arg 3
    'mapname',    [],  ...  % arg 4
    'datarange',  [],  ...  % arg 5
    'showrange',  [],  ...  % arg 6
    'dual',       [],  ...  % arg 7
    'colourgap',  [],  ...  % arg 8
    'coption',    [],  ...  % arg 9
    'mapsize',    [],  ...  % arg 10
    'imgsize',    [],  ...  % arg 11
    'templatemtl',[]);      % arg 12
fields = fieldnames(v);
nargin = numel(varargin);
for a = 1:nargin,
    v.(fields{a}) = varargin{a};
    if (a >= 5) && (a <= 11) && ischar(v.(fields{a})),
        v.(fields{a}) = eval(v.(fields{a}));
    end
end

% Accept some defaults if needed
for a = [4 7:11],
    if isempty(v.(fields{a})),
        v.(fields{a}) = d.(fields{a});
    end
end

% Read the data file and the surface geometry
dpx = dpxread(v.dpxfile);
[vtx,fac] = srfread(v.srffile);
nX = size(dpx,1);
nV = size(vtx,1);
nF = size(fac,1);
didx0 = ~(isinf(dpx) | isnan(dpx)); % indices to be used (not Inf or NaN)

% Verify if this is facewise or vertexwise data
if nX == nV,
    fprintf('Working with vertexwise data.\n');
    facewise = false;
elseif nX == nF,
    fprintf('Working with facewise data.\n');
    facewise = true;
else
    error('The data does not match the surface.');
end

% Define ranges to show
dpxdat = dpx(didx0);
if isempty(v.datarange),
    dpxmin = min(dpxdat);
    dpxmax = max(dpxdat);
else
    dpxmin = v.datarange(1);
    dpxmax = v.datarange(2);
    dpxdat(dpxdat < dpxmin) = dpxmin;
    dpxdat(dpxdat > dpxmax) = dpxmax;
    dpx(didx0) = dpxdat;
end
if isempty(v.showrange),
    v.showrange = [dpxmin dpxmax];
end
shwmin = v.showrange(1);
shwmax = v.showrange(2);
if shwmin < dpxmin,
    shwmin = dpxmin;
end
if shwmax > dpxmax,
    shwmax = dpxmax;
end

% Make the infinites with the same colour as the max
dpx(isinf(dpx) & dpx < 0) = dpxmin;
dpx(isinf(dpx) & dpx > 0) = dpxmax;
didx = ~ isnan(dpx); % indices to be used (not Inf or NaN)

% Some sanity checks
if (dpxmax == dpxmin) || (shwmax == shwmin),
    error('Invalid intervals for datarange and/or showrange');
end
if ~ facewise && v.mapsize > 2^24,  v.mapsize = 2^24;  end
if   facewise && v.mapsize > 32767, v.mapsize = 32767; end

% Define the actual colourmap size.
if ~ v.dual,
    mapsize = ceil((dpxmax - dpxmin) * ...
        v.mapsize / (shwmax - shwmin));
else
    mapsize = ceil((dpxmax - dpxmin) * (v.mapsize - 1) / ...
        ((dpxmax - dpxmin) - (shwmax - shwmin)));
end

% Define the colour indices
cidx = zeros(size(dpx));
cidx(didx) = ceil(1 + (dpx(didx) - dpxmin) ...
    * (mapsize - 1) / (dpxmax - dpxmin));

% Define the colourmap
rgb = eval(sprintf('%s(%f);',v.mapname,mapsize));
mapsize = size(rgb,1);
if any(~didx) || v.dual || ~v.coption,
    rgb = [rgb ; v.colourgap];
end

% Define the colour indices that will be shown within the colour window
if ~ v.dual,
    tmp = [shwmin shwmax];
    sidx = ceil(1 + (tmp - dpxmin) * (mapsize - 1) / (dpxmax - dpxmin));
    if v.coption,
        cidx(cidx < sidx(1)) = sidx(1);
        cidx(cidx > sidx(2)) = sidx(2);
    else
        cidx(cidx < sidx(1) | cidx > sidx(2)) = mapsize + 1;
    end
else
    if v.coption,
        shwhalf = (shwmin + shwmax)/2;
        cidxgap = ceil(1 + (shwhalf - dpxmin) ...
            * (mapsize - 1) / (dpxmax - dpxmin));
        cidx1 = ceil(1 + (dpx(dpx < shwmin) - dpxmin) ...
            * (cidxgap - 1) / (shwmin - dpxmin));
        cidx2 = ceil(cidxgap + (dpx(dpx > shwmax) - shwmax) * ...
            (mapsize - cidxgap) / (dpxmax - shwmax));
        cidx = zeros(size(dpx));
        cidx(dpx < shwmin) = cidx1;
        cidx(dpx > shwmax) = cidx2;
        cidx(dpx > shwmin & dpx < shwmax) = mapsize + 1;
    else
        tmp = [shwmin (shwmin+shwmax)/2 shwmax];
        sidx = ceil(1 + (tmp - dpxmin) * ...
            (mapsize - 1) / (dpxmax - dpxmin));
        cidx(cidx >= sidx(2) & cidx < sidx(3)) = mapsize + 1;
        cidx(cidx <= sidx(2) & cidx > sidx(1)) = mapsize + 1;
    end
end

% NaN/Inf are always outside the colourscale
cidx(~didx) = mapsize + 1;

% Save the surface maps, as OBJ/MTL for facewise or PLY for vertexwise
if facewise,

    % Create an unique list to generate the MTL
    midx = sort(unique(cidx));

    % Sort faces according to material
    [cidx,ix] = sort(cidx);
    fac = fac(ix,:);

    % Load a template MTL file or use defaults
    if isempty(v.templatemtl),
        v.mtl = d.mtl;
    else
        v.mtl = mtlread(v.templatemtl);
    end

    % Create the material library, as a struct, to be saved later
    for m = 1:numel(midx),
        mtl.(sprintf('m%0.6u',midx(m)))    = v.mtl;           % Clone
        mtl.(sprintf('m%0.6u',midx(m))).Kd = rgb(midx(m),:);  % Change the diffuse colour
    end

    % Save material library (MTL)
    mtlfile = sprintf('%s.mtl',v.oprefix);
    fprintf('Saving MTL: %s\n',mtlfile);
    mtlwrite(mtl,mtlfile);

    % Save surface (OBJ)
    objfile = sprintf('%s.obj',v.oprefix);
    fprintf('Saving OBJ: %s\n',objfile);
    fido = fopen(objfile,'w');
    fprintf(fido,'mtllib %s.mtl\n',v.oprefix);
    fprintf(fido,'v %g %g %g\n',vtx');
    for m = 1:numel(midx),
        fprintf(fido,'usemtl m%0.6u\n',midx(m));
        fprintf(fido,'f %u %u %u\n',fac(cidx == midx(m),:)');
    end
    fclose(fido);

else

    % Prepare vertices, colours and faces
    rgb = double(uint8(255*rgb));
    vtx2ply = [vtx rgb(cidx,1) rgb(cidx,2) rgb(cidx,3)];
    fac2ply = [ones(nF,1)*3 fac-1];

    % Save surface (PLY)
    plyfile = sprintf('%s.ply',v.oprefix);
    fprintf('Saving PLY: %s\n',plyfile);
    fid = fopen(plyfile,'w');
    fprintf(fid,'ply\n');
    fprintf(fid,'format ascii 1.0\n');
    fprintf(fid,'element vertex %s\n',num2str(nV));
    fprintf(fid,'property float x\n');
    fprintf(fid,'property float y\n');
    fprintf(fid,'property float z\n');
    fprintf(fid,'property uchar red\n');
    fprintf(fid,'property uchar green\n');
    fprintf(fid,'property uchar blue\n');
    fprintf(fid,'element face %s\n',num2str(nF));
    fprintf(fid,'property list uchar int vertex_index\n');
    fprintf(fid,'end_header\n');
    fprintf(fid,'%g %g %g %d %d %d\n',vtx2ply');
    fprintf(fid,'%d %d %d %d\n',fac2ply');
    fclose(fid);
   
end

% Now for the colourbar
%if ~isoctave && all(v.imgsize),
if all(v.imgsize),
    
    % Simulate data for display purposes
    dpx = linspace(dpxmin,dpxmax,max(v.imgsize))';

    % Define the colour indices
    cidx = ceil(1 + (dpx - dpxmin) ...
        * (mapsize - 1) / (dpxmax - dpxmin));

    % Define the colour indices that will be shown within the colour window
    if ~ v.dual,
        if v.coption,
            cidx(cidx < sidx(1)) = sidx(1);
            cidx(cidx > sidx(2)) = sidx(2);
        else
            cidx(cidx < sidx(1) | cidx > sidx(2)) = mapsize + 1;
        end
    else
        if v.coption,
            cidxgap = ceil(1 + (shwhalf - dpxmin) ...
                * (mapsize - 1) / (dpxmax - dpxmin));
            cidx1 = ceil(1 + (dpx(dpx < shwmin) - dpxmin) ...
                * (cidxgap - 1) / (shwmin - dpxmin));
            cidx2 = ceil(cidxgap + (dpx(dpx > shwmax) - shwmax) * ...
                (mapsize - cidxgap) / (dpxmax - shwmax));
            cidx = zeros(size(dpx));
            cidx(dpx < shwmin) = cidx1;
            cidx(dpx > shwmax) = cidx2;
            cidx(dpx > shwmin & dpx < shwmax) = mapsize + 1;
        else
            cidx(cidx >= sidx(2) & cidx < sidx(3)) = mapsize + 1;
            cidx(cidx <= sidx(2) & cidx > sidx(1)) = mapsize + 1;
        end
    end
    
    % Save the colourbar, as PNG
    pngfile = sprintf('%s.png',v.oprefix);
    fprintf('Saving PNG: %s\n',pngfile);
    png = zeros([max(v.imgsize) 1 3]);
    for c = 1:3,
        png(:,1,c) = rgb(cidx,c);
    end
    png = repmat(png,[1 min(v.imgsize) 1]);
    if v.imgsize(1) > v.imgsize(2),
        png = permute(png,[2 1 3]);
    end
    imwrite(uint8(png),pngfile,'png');
end

% That's it!
