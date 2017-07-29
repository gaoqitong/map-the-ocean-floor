function [arrivals] = arrwav(filename)

% *** Generates an Arrivals Matrix from a Wav File ***

[amp, fs] = audioread(strcat(filename,'.wav'));

ind = find(amp>0);
arrivals=zeros(2,length(ind));
for k=1:length(ind);
    arrivals(1,k)=fs*ind(k);
    arrivals(2,k)=amp(ind(k));
end

