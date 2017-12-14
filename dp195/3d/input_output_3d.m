% l = dir('*.bty');
% output = zeros(42,numel(l));
% input_compressed = zeros(237568,numel(l));
% for i=1:numel(l)
%     name_bty = l(i).name;
%     name = name_bty(1:end-4);
%     name_wav = strcat(name, '_Pos1_rts_Rd_1_Rr_1.wav');
%     input_compressed(:,i) = myDCT(name_wav);
%     output(:,i) = readbty(name_bty,'*',inf,inf,0);
% end
%net1
l = dir('*.bty');
output = zeros(41*41,length(l)*2);
input_full = zeros(240000,length(l)*2);
for i=1:length(l)
    name_bty = l(i).name;
    name = name_bty(1:end-4);
    name_wav = strcat(name, '_rts_Rd_1_Rr_1.wav');
    input_full(:,i) = audioread(name_wav);
    input_full(:,length(l)+i) = fliplr(audioread(name_wav));
    output(:,i) = reshape(readbty3d(name_bty),[],1);
    output(:,length(l)+i) = fliplr(reshape(readbty3d(name_bty),[],1));
end