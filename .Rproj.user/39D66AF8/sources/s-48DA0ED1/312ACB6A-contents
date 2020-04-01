% Converts aseg to ply files
% assumes that "~/Brainder/atlas2srf.sh -s "<subject>" " hase been runned for the target subject
% wrapper, based on A. Winkler scripts.  
% correspondence between labelnames and asegnames might need to be switched off depending on inputting files. Should work for any FS annot file

subj = 'fsaverage'
wdir = '/ninet/tsd-evs.tsd.usit.no/p23/home/p23-didacvp/GWC/TEST/'
indir = [wdir filesep subj filesep 'ascii']
outdir = '/net/tsd-evs.tsd.usit.no/p23/home/p23-didacvp/GGSEG/PlyData'
%ctabdir = [wdir filesep subj filesep 'mri']
suffix='JHU'
cd(indir)


% starting main routine %
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



