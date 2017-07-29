Initializing;
filename = 'flat';
global k;
k = 0;
parfor i = 1 : 100000
    
   count_wav( filename, i ); 
    
end

save('NumofWavs_correct.mat','k');
