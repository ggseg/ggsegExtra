% #!/usr/bin/octave -q
function smoothdpx(varargin)
% Smooth data per face (DPF) or data per vertex (DPV) with a Gaussian kernel
% of a specified width. The user must supply a spherical reference surface.
% 
% Usage 1:
% smoothdpx -i inputdpx -o outputdpx -s srffile -f fwhm [-m matrix.mat]
% 
% Usage 2:
% smoothdpx -s srffile -f fwhm -m matrix.mat [-facewise]
% 
% -i filename : Input DPV/DPF file, which values will be spatially smoothed.
% -o filename : Output DPV/DPF file, with smoothed data.
% -s filename : Surface reference file.
% -f fwhm     : Full-Width at Half Maximum of the Gaussian filter.
% -m filename : Filename for the smoothing matrix. It is saved in binary
%               '.mat' format, as a sparse matrix.
%               The smoothing matrix can be used with rpncalc for
%               very fast smoothing.
% -facewise   : If no input is given and -m is used, this forces the
%               surface vertices to be replaces by their barycenters.
% 
% _____________________________________
% Anderson M. Winkler
% Yale University / Institute of Living
% Aug/2011 (first version)
% Nov/2014 (this version)
% http://brainder.org

try %#ok
    % Get the inputs
    varargin = argv();
    
    % Disable memory dump on SIGTERM
    sigterm_dumps_octave_core(0);
    
    % Print usage if no inputs are given
    if isempty(varargin) || strcmp(varargin{1},'-q'),
        fprintf('Smooth data per face (DPF) or data per vertex (DPV) with a Gaussian kernel\n');
        fprintf('of a specified width. The user must supply a spherical reference surface.\n');
        fprintf('\n');
        fprintf('Usage 1:\n');
        fprintf('smoothdpx -i input.dpx -o output.dpx -s surf.srf -f fwhm [-m matrix.mat]\n');
        fprintf('\n');
        fprintf('Usage 2:\n');
        fprintf('smoothdpx -s input.dpx -f fwhm -m matrix.mat [-facewise]\n');
        fprintf('\n');
        fprintf('-i filename : Input DPV/DPF file, which values will be spatially smoothed.\n');
        fprintf('-o filename : Output DPV/DPF file, with smoothed data.\n');
        fprintf('-s filename : Surface reference file.\n');
        fprintf('-f fwhm     : Full-Width at Half Maximum of the Gaussian filter.\n');
        fprintf('-m filename : Filename for the smoothing matrix. It is saved in binary\n');
        fprintf('              ''.mat'' format, as a sparse matrix.\n');
        fprintf('              The smoothing matrix can be used with rpncalc for\n');
        fprintf('              very fast smoothing.\n');
        fprintf('-facewise   : If no input is given and -m is used, this forces the\n');
        fprintf('              surface vertices to be replaces by their barycenters.\n');
        fprintf('\n');
        fprintf('_____________________________________\n');
        fprintf('Anderson M. Winkler\n');
        fprintf('Yale University / Institute of Living\n');
        fprintf('Aug/2011 (first version)\n');
        fprintf('Nov/2014 (this version)\n');
        fprintf('http://brainder.org\n');
        return;
    end
    
    % To save correctly in the right version below
    isoct = true;
end

% Truncate the kernel after how many FWHMs?
fwhmtrunc = 2;

% Take input arguments
opts.facewise = false;
a = 1;
while a <= nargin,
    switch varargin{a},
        case '-i',
            opts.i = varargin{a+1};
            a = a + 2;
        case '-o',
            opts.o = varargin{a+1};
            a = a + 2;
        case '-s',
            opts.s = varargin{a+1};
            a = a + 2;
        case '-f',
            opts.f = varargin{a+1};
            a = a + 2;
        case '-m',
            opts.m = varargin{a+1};
            a = a + 2;
        case '-facewise',
            opts.facewise = true;
            a = a + 1;
    end
end

% Check what to do
dosmooth = false;
domatrix = false;
if all( [ ...
        isfield(opts,'i') ...
        isfield(opts,'o') ...
        isfield(opts,'s') ...
        isfield(opts,'f')]),
    dosmooth = true;
end
if all( [ ...
        isfield(opts,'s') ,...
        isfield(opts,'f') ...
        isfield(opts,'m')]),
    domatrix = true;
end
if ~dosmooth && ~domatrix,
    error('Incorrect arguments. Consult the usage information.');
end
if ischar(opts.f),
    opts.f = eval(opts.f);
end

% Read input data and reference surface (sphere)
[vtxi,faci]      = srfread(opts.s);
nV = size(vtxi,1);  % Number of vertices
nF = size(faci,1);  % Number of faces

% If it's going to smooth
if dosmooth,
    
    % Load DPX file
    [dpxi,crdi,idxi] = dpxread(opts.i);
    nX = numel(dpxi);   % Number of datapoints to smooth
    
    % Check if facewise or vertexwise. This overrides the -facewise option
    if nX == nF,
        fprintf('Smoothing facewise data, FWHM=%g\n',opts.f);
        opts.facewise = true;
    elseif nX == nV,
        fprintf('Smoothing vertexwise data, FWHM=%g\n',opts.f);
        opts.facewise = false;
    else
        error('Data to smooth does not match surface geometry');
    end
    
    % Where to store result
    dpxo = zeros(size(dpxi));
end

% Compute the barycenters
if opts.facewise,
    facvtx = [vtxi(faci(:,1),:) vtxi(faci(:,2),:) vtxi(faci(:,3),:)];
    vtxi   = [mean(facvtx(:,[1 4 7]),2) ...
        mean(facvtx(:,[2 5 8]),2) mean(facvtx(:,[3 6 9]),2)];
end

% Make sure nX has been defined even if the only input was a surface
nX = size(vtxi,1);

% Get an approx to the sphere radius
[~,~,Ri] = cart2sph(vtxi(:,1),vtxi(:,2),vtxi(:,3));
R = mean(Ri);

% Precalculate some constants for the kernel
sigma = opts.f/sqrt(8*log(2));
cte1  = 1/sigma/sqrt(2*pi);
cte2  = -1/(2*sigma^2);

% Reserve space for the smoothing matrix
if domatrix,
    % Area of the spherical cap under the filter is:
    % 2*pi*R^2*(1-cos(fwhmtrunc*opts.f/R))
    % Area of the sphere is 4*pi*R^2
    % The estimated number of non-zero elements is therefore:
    nnz = ceil(nX*nX*(1-cos(fwhmtrunc*opts.f/R))/2);
    smoothmtx = spalloc(nX,nX,nnz);
end

% Loop over datapoints (vertices or centers of faces)
for v = 1:nX,
    
    % Geodesic distance between current vertex and all other vertices
    vtx = repmat(vtxi(v,:),[nX 1]);
    G   = geodist(vtx,vtxi,R);
    idx = G < (fwhmtrunc*opts.f);
    
    % Solve the filter for the vertex locations (distances)
    K = cte1*exp(cte2*G(idx).^2);
    K = K/sum(K);
    
    % Weight
    if dosmooth,
        dpxo(v) = sum(dpxi(idx).*K);
    end
    
    % Put the kernel weights in the smoothing matrix
    if domatrix,
        smoothmtx(v,idx) = K; %#ok (it's never quick with sparse)
    end
end

% If the filter is huge, there may be tiny imaginary parts.
% Take the real part only & save
if dosmooth,
    dpxo = real(dpxo);
    dpxwrite(opts.o,dpxo,crdi,idxi);
end

% Save the smoothing matrix
if domatrix,
    if isoct,
        save(opts.m,'smoothmtx');
    else
        save(opts.m,'smoothmtx','-v7.3');
    end
end
