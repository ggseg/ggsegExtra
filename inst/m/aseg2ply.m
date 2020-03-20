% Converts aseg to ply files
% assumes that "~/Brainder/aseg2srf -s "<subject>" " hase been runned for the target subject

subj = 'fsaverage5'
wdir = '/net/tsd-evs.tsd.usit.no/p23/home/p23-didacvp/GWC/TEST/'
indir = [wdir filesep subj filesep 'ascii']
outdir = '/net/tsd-evs.tsd.usit.no/p23/home/p23-didacvp/GGSEG/PlyData'
%ctabdir = [wdir filesep subj filesep 'mri']
suffix='aseg'
cd(indir)


% starting main routing %
% ..................... %


list = dir([ suffix '_*.srf'])
list_save ={};
IDaseg = []

for i=1:length(list)
    [~, name, ~] = fileparts(list(i).name)
    [idx]=strsplit(name,'_');
    IDaseg = [IDaseg, str2num(idx{2})];
    % append list of files to merge
    list_save = {list_save{:}, strrep(list(i).name,'.srf','.ply')};
    % Compute the area per face or per vertex for an ASCII surface file and save as a DPV file.
    srf2area([name '.srf'], [name '.dpv'],'dpv')
    % generate ply maps of the full surface
    dpx2map([name '.dpv'], [name '.srf'], name)
end

% get lable names
I = [wdir filesep 'ASegStatsLUT.txt']
T = readtable(I,'HeaderLines',6, 'Format','%d%s', 'ReadVariableNames',false, 'Delimiter', 'tab')
isaseg = ismember(table2array(T(:,1)), IDaseg);
T = T(isaseg, :)
writetable(T, 'annot2filename.csv')


zip([suffix '.' subj],{list_save{:}, 'annot2filename.csv'})
copyfile([suffix '.' subj '.zip'], [outdir filesep suffix '.' subj '.zip'])

% Code to visualize ply data

%ptCloud = pcread('test2.ply');
%pcshw(ptCloud);

%[Tri,Pts] = ply_read('lh.white_roi.0004.ply','tri');
%trisurf(Tri,Pts(:,1),Pts(:,2),Pts(:,3));
%colormap(gray); axis equal;




% dpx2map
% function dpx2map(varargin)
% Generate a surface map of data stored as DPV or DPF, using a custom
% colourscale. The result is saved as either OBJ/MTL pair or PLY, and
% can be imported for scene construction and rendering in 3D applications.
% In addition, saves also a PNG file represending the colourscale (without
% labels).
%
% Usage:
% dpx2map(dpxfile,srffile,oprefix,mapname,datarange,showrange,...
%           dual,colourgap,coption,mapsize,imgsize,templatemtl)

%srf2area
% function srf2area(varargin)
% Compute the area per face or per vertex for an ASCII surface file and
% save as a DPV or DPF (aka curvature) file.
%
% Usage:
% srf2area(srffile,areafile,meas)

