% TrenchGen
clear

numfil=100; % Pick a number with an integer square root for the code to work
noise=1;
variance=5;
DoArr=1;

h=linspace(725,950,sqrt(numfil));
w=linspace(0.310,0.4,sqrt(numfil));


for k=1:sqrt(numfil);
    
    for m=1:sqrt(numfil);
        if (noise==1)
            filename = strcat('Trench-Noise-h',num2str(h(k)),'-w',num2str(w(m)*1000));
        else
            filename = strcat('Trench-h',num2str(h(k)),'-w',num2str(w(m)*1000));
        end
        fileid = fopen(strcat(filename,'.bty'),'w');
        points = cell(1,6);
        
        if (noise==1)
            x = randi([50 100],1,1);
            points = cell(1,6+x);
        end
        
        points{1}=[-2.997 700];
        points{end}=[2.997 700];
        
        if (noise==1)
            surf = 3000+2*h(k);
            
            a1 = round(length(points)*(1-w(m)/2)*1000/surf,0);
            a2 = round(length(points)*h(k)/surf+a1,0);
            a3 = round(length(points)*w(m)*1000/surf+a2,0);
            a4 = round(length(points)*h(k)/surf+a3,0);
            
            points{a1}=[1-w(m)/2 700];
            points{a2}=[1-w(m)/2 h(k)];
            points{a3}=[1+w(m)/2 h(k)];
            points{a4}=[1+w(m)/2 700];
            
            for p1=2:(a1-1)
                dep = randi([-1*variance variance],1,1);
                pos = randi([-2996 round((1-w(m)/2)*1000-1,0)],1,1)*0.001;
                points{p1}=[pos 700+dep];
            end
            dep = linspace(700,h(k)+1,a2-a1-1);
            for p2=(a1+1):(a2-1)
                pos = randi([-1*variance variance],1,1)*0.001;
                points{p2}=[(1-w(m)/2) dep(p2-a1)];
            end
            for p3=(a2+1):(a3-1)
                dep = randi([-1*variance variance],1,1);
                pos = randi([round((1-w(m)/2)*1000+1,0) round((1+w(m)/2)*1000,0)],1,1)*0.001;
                points{p3}=[pos h(k)+dep];
            end
            dep = linspace(700,h(k)+1,a4-a3-1);
            for p4=(a3+1):(a4-1)
                pos = randi([-1*variance variance],1,1)*0.001;
                points{p4}=[(1+w(m)/2) dep(p4-a3)];
            end
            for p5=a4+1:length(points)-1
                dep = randi([-1*variance variance],1,1);
                pos = randi([round((1+w(m)/2)*1000+1,0) 2996],1,1)*0.001;
                points{p5}=[pos 700+dep];
            end

        else
            points{2}=[1-w(m)/2 700];
            points{3}=[1-w(m)/2 h(k)];
            points{4}=[1+w(m)/2 h(k)];
            points{5}=[1+w(m)/2 700];
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
        copyfile('Trench1.env',strcat(filename,'.env'));
        
        if DoArr==1
            driftdelay(filename,100,10,100,950)
        end
    end
    
end