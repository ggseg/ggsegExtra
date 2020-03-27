function srf2area(varargin)
% Compute the area per face or per vertex for an ASCII surface file and
% save as a DPV or DPF (aka curvature) file.
%
% Usage:
% srf2area(srffile,areafile,meas)
% 
% - srffile  = Input surface file, in ASCII format
% - areafile = Output file, as a DPV or DPF ASCII format. For 'dpv', the
%              format is the conventional FreeSurfer's curvature file.
%              For 'dpf' it contains face indices instead of vertex indices, 
%              and vertex indices instead of vertex coordinates.
% - meas     = It can be either 'dpv' for area per vertex or
%              'dpf' for area per face. The area per vertex is simply
%              the sum of the 'dpf' for all faces that meet at the vertex
%              divided by 3. Use 'dpf' for studies of area
%              between subjects. Default is 'dpf'.
%
% _____________________________________
% Anderson M. Winkler
% Yale University / Institute of Living
% Jan/2011
% http://brainder.org

% Do some OCTAVE stuff, but use TRY to ensure MATLAB compatibility
try
    % Get the inputs
    varargin = argv();

    % Disable memory dump on SIGTERM
    sigterm_dumps_octave_core(0);

    % Print usage if no inputs are given
    if isempty(varargin) || strcmp(varargin{1},'-q'),

        fprintf('Compute the area per face or per vertex for an ASCII surface file and\n');
        fprintf('save as a DPV or DPF (aka curvature) file.\n');
        fprintf('\n');
        fprintf('Usage:\n');
        fprintf('srf2area <srffile> <areafile> [meas]\n');
        fprintf('\n');
        fprintf('- srffile  = Input surface file, in ASCII format\n');
        fprintf('- areafile = Output file, as a DPV or DPF ASCII format. For ''dpv'', the\n');
        fprintf('             format is the conventional FreeSurfer''s curvature file.\n');
        fprintf('             For ''dpf'' it contains face indices instead of vertex indices, \n');
        fprintf('             and vertex indices instead of vertex coordinates.\n');
        fprintf('- meas     = It can be either ''dpv'' for area per vertex or\n');
        fprintf('             ''dpf'' for area per face. The area per vertex is simply\n');
        fprintf('             the sum of the ''dpf'' for all faces that meet at the vertex\n');
        fprintf('             divided by 3. Use ''dpf'' for studies of area\n');
        fprintf('             between subjects. Default is ''dpf''.\n');
        fprintf('\n');
        fprintf('_____________________________________\n');
        fprintf('Anderson M. Winkler\n');
        fprintf('Yale University / Institute of Living\n');
        fprintf('Jan/2011\n');
        fprintf('http://brainder.org\n');
        return;
    end
end

% Defaults
d.fsrf = [];
d.fdpx = [];
d.meas = 'dpf';

% Accept user arguments
fields = fieldnames(d);
nargin = numel(varargin);
for a = 1:nargin,
    d.(fields{a}) = varargin{a};
end

% Read the surface file
[vtx,fac] = srfread(d.fsrf);
nV = size(vtx,1);
nF = size(fac,1);

% Compute area per face (DPF)
facvtx = [vtx(fac(:,1),:) vtx(fac(:,2),:) vtx(fac(:,3),:)];
facvtx0(:,1:6) = facvtx(:,1:6) - [facvtx(:,7:9) facvtx(:,7:9)];  % Place 3rd vtx at origin
cp = cross(facvtx0(:,1:3),facvtx0(:,4:6),2);                     % Cross product
dpf = sqrt(sum(cp.^2,2))./2;                                     % Half of the norm
fprintf('Total area (facewise): %g\n',sum(dpf));

if strcmpi(d.meas,'dpf')
    
    % Prepare DPF to save
    tosave = [(0:nF-1)' fac dpf ];
    
elseif strcmpi(d.meas,'dpv')

    % Compute area per vertex (DPV)
    dpv = zeros(nV,1);
    
    % For speed, divide the dpf by 3.
    dpf3 = dpf/3;

    % Redistribute!
    for f = 1:nF,
        dpv(fac(f,:)) = dpv(fac(f,:)) + dpf3(f);
    end
    fprintf('Total area (vertexwise): %g\n',sum(dpv));

    % Prepare DPV to save
    tosave = [(0:nV-1)' vtx dpv];
end

% Save the result as a DPV/DPF file
if ~isempty(d.fdpx),
    fid = fopen(d.fdpx,'w');
    fprintf(fid,'%0.3d %g %g %g %0.16f\n',tosave');
    fclose(fid);
end
