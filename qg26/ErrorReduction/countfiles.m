k = zeros(1000, 1);
parfor i = 1 : 1000
    
        
    [~,cmdout] = dos(['cd /s/qitong/btyenv7/', num2str(i), '/ && ls -s | grep ^ - | wc -l']);
    if size(cmdout,2) >= 5
    
        if strcmp(cmdout(1:5),'12601')
             k(i) = 1;
             disp(i);
        end
    
    end
    
end

	save('countJuly24th.mat');
