function plotfrequency(filename)

filename = [ filename , '.wav' ];

[wave,fs]=audioread(filename); % read file into memory */
sound(wave,fs); % see what it sounds like */
t=0:1/fs:(length(wave)-1)/fs; % and get sampling frequency */

 figure
          plot(t,wave);
          title('Wave File');
          ylabel('Amplitude');
          xlabel('Length (in seconds)');
 % graph it ? try zooming while its up?not much visible until you do*/
n=length(wave)-1; 
f=0:fs/n:fs;
wavefft=abs(fft(wave)); % perform Fourier Transform *

figure
          plot(f,wavefft); % plot Fourier Transform */
          xlabel('Frequency in Hz');
          ylabel('Magnitude');
          title('The Wave FFT');
end
