% SlopeGen
clear

numfil=100;
noise=1;
variance=5;
DoArr=1;

m=linspace(-0.10,0.10,numfil); % meters/meters

for k=1:numfil;
        if (noise==1)
            filename = strcat('Slope-Noise-m',num2str(m(k)));
        else
            filename = strcat('Slope-m',num2str(m(k)));
        end
        fileid = fopen(strcat(filename,'.bty'),'w');
        points = cell(1,2);
        
        if (noise==1)
            x = randi([50 100],1,1);
            points = cell(1,2+x);
        end
        
        points{1}=[-2.997 970];
        points{end}=[2.997 970+m(k)*6000];
        
        if (noise==1)
            for p1=2:length(points)-1
                dep = randi([-1*variance variance],1,1);
                pos = randi([-2996 2996],1,1)*0.001;
                points{p1}=[pos 970+dep+m(k)*(pos+3)*1000];
            end
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
        copyfile('Slope1.env',strcat(filename,'.env')); 
        
        if DoArr==1
            driftdelay(filename,100,10,100,970+m(k)*(4000))
        end
end