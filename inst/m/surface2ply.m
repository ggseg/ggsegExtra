% Converts surface to ply files
% using white surface as base
% subj2ascii <subj_id> needs to be run before the script
% wrapper based on A. Winkler scripts 
% correspondence between labelnames and annotnames might need to be switched off depending on inputting files. 
% Should work for any FS annot file

subj = 'fsaverage5'
wdir = '/your/folder/'
indir = [wdir filesep subj filesep 'ascii']
labeldir = [wdir filesep subj filesep 'label']

outdir = '/output/folder'
hemi = {'lh','rh'}
surface = 'inflated'
aparc_list= {'gclust_area','gclust_thickness'};

cd(indir)
for j=1:length(aparc_list)
    aparc = aparc_list{j}
    
    for h=1:length(hemi)
        list_rm =dir([ hemi{h} '.' surface '_*srf'])
        if length(list_rm) ~= 0
            delete(list_rm.name)
        end

         % convert annotation to dpv files
        annot2dpv([labeldir filesep hemi{h} '.' aparc '.annot'], [indir filesep hemi{h} '.' aparc '.annot.dpv'])
        
        % split surface based on white labels
        splitsrf([hemi{h} '.' surface '.srf'], [hemi{h} '.' aparc '.annot.dpv'], [hemi{h} '.' surface '_roi'])
        
        % generate ply maps of the full surface
        dpx2map([hemi{h} '.' aparc '.annot.dpv'], [hemi{h} '.' surface '.srf'], [hemi{h} '.' aparc '.annot'])


        list = dir([ hemi{h} '.' surface '_*srf'])
        list_save ={[hemi{h} '.' aparc '.annot.ply']}
        for i=1:length(list)
            [~, name, ~] = fileparts(list(i).name)
            % append list of files to merge
            list_save = {list_save{:}, strrep(list(i).name,'.srf','.ply')}
            % Compute the area per face or per vertex for an ASCII surface file and save as a DPV file.
            srf2area([name '.srf'], [name '.dpv'],'dpv')
            % generate ply maps of the full surface
            dpx2map([name '.dpv'], [name '.srf'], name)
        end

        % get label names
        [vertices label ctab] = read_annotation([labeldir filesep hemi{h} '.' aparc '.annot']);

        % if more labels names than labels in annot
        if length(ctab.struct_names) ~= length(list)
            idx = ismember(ctab.table(:,5), label)    
            ctab.struct_names = ctab.struct_names(idx)
        end

        % create correspondence between file names and label names
        ctab2 = {[hemi{h} '.' aparc '.annot'], ctab.struct_names{:}}
        T =table(transpose(list_save), transpose(ctab2),'VariableNames', {'filename','annot'})
        writetable(T, 'annot2filename.csv')

        zip([hemi{h} '.' surface '.' subj],{list_save{:}, 'annot2filename.csv'})
        copyfile([hemi{h} '.' surface '.' subj '.zip'], [outdir filesep hemi{h} '.' aparc '.' surface '.' subj '.zip'])
    end
end

