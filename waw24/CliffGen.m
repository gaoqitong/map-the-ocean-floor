% CliffGen
clear

numfil=100;
noise=1;
variance=5;
DoArr=1;

h=linspace(50,950,numfil);

for k=1:numfil;
        if (noise==1)
            filename = strcat('Cliff-Noise-h',num2str(h(k)));
        else
            filename = strcat('Cliff-h',num2str(h(k)));
        end
        fileid = fopen(strcat(filename,'.bty'),'w');
        points = cell(1,4);
        
        if (noise==1)
            x = randi([50 100],1,1);
            points = cell(1,4+x);
        end
        
        points{1}=[-2.997 970];
        points{end}=[2.997 h(k)];
        
        if (noise==1)
            surf = 3000+h(k);
            
            a1 = round(length(points)*1000/surf,0);
            a2 = round(length(points)*h(k)/surf+a1,0);
      
            points{a1}=[1 970];
            points{a2}=[1 h(k)];
            
            for p1=2:(a1-1)
                dep = randi([-1*variance variance],1,1);
                pos = randi([-2996 999],1,1)*0.001;
                points{p1}=[pos 970+dep];
            end
            dep = linspace(969,h(k)+1,a2-a1-1);
            for p2=(a1+1):(a2-1)
                pos = randi([-1*variance variance],1,1)*0.001;
                points{p2}=[1 dep(p2-a1)];
            end
            for p3=(a2+1):length(points)-1;
                dep = randi([-1*variance variance],1,1);
                pos = randi([1001 2996],1,1)*0.001;
                points{p3}=[pos h(k)+dep];
            end

        else
            points{2}=[1 970];
            points{3}=[1 h(k)];
        end
        
        s=zeros(1,length(points));
        for q=1:length(points)
            s(q)=points{q}(1);
        end
        [~,S]=sort(s);
        points = points(S);
        
        fprintf(fileid,strcat('''L''\n',num2str(length(points)),'\n'));
        format ='%4.3f %4.3f \n';
        for n=1:length(points)
            fprintf(fileid,format,points{n}(1),points{n}(2));
        end
        fclose(fileid);
        copyfile('Cliff1.env',strcat(filename,'.env'));  
        
        if DoArr==1
            driftdelay(filename,200,10,100,h(k)-50)
        end
end