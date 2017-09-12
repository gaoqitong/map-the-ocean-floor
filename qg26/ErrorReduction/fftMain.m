close all
fs = 1000;
nfft = 1000;
flat = ones(fs, 1);
flat = flat*800;
noise = randn(fs, 1) * 1000;
bty = flat + noise;
%% FFT
fft_bty = fft(bty,nfft);
f = (0:nfft-1)*fs/nfft;
subplot(2,1,1)
plot(bty,'linewidth',2)
subplot(2,1,2)
plot(f,abs(fft_bty),'linewidth',2)
xlabel('Frequency')
ylabel('Magnitude')
%% Cut and iFFT
fft_bty_aftercut = abs(fft_bty);
fft_bty_aftercut(5:end) = 0;
bty_aftercut = ifft(fft_bty_aftercut);
bty_aftercut = abs(bty_aftercut);
figure
plot(bty_aftercut,'linewidth',2)
save('bty_aftercut.mat','bty_aftercut');
%% Re-add Noise
load bty_aftercut.mat
fft_bty_aftercut = abs(fft(bty_aftercut));
fft_bty_aftercut(10) = 5*10^3;
fft_bty_aftercut(50) = 1*10^3;
fft_bty_aftercut(100) = 7*10^2;
% fft_bty_aftercut(500) = 1*10^2;
bty_aftercut = abs(ifft(fft_bty_aftercut));
figure
plot(bty_aftercut,'linewidth',2)
figure
plot(abs(fft_bty_aftercut),'linewidth',2)
xlabel('Frequency')
ylabel('Magnitude')