function H = hglampspctrl(k)
% 2016-12-23, 1st built, Wang Weihua
% Plot out mercury lamp spectral of 2 ms exposure time for the specific measured day.
% eg: filepath='/home/wien/Octave/flameDOAS/calibrations/hg_2ms_1000av_160508.STD';
% k is the date number of 6,7,8,10,11,13,15,16,17.

filepath=[];
dirpath='/home/wien/Octave/flameDOAS/calibrations/';
if (1<=k && k<=9)
filepath = [dirpath 'hg_2ms_1000av_16050' num2str(k) '.STD'];
elseif (10<=k && k<=31)
filepath = [dirpath 'hg_2ms_1000av_1605' num2str(k) '.STD'];
endif

F = csvread(filepath); % F: Hg lamp spectra on kth day.
FU = F(4:2051,:);
figure;
plot(FU);
xlim([0,2050]);
title(['Hg lamp spectra on May ' num2str(k)]); 
xlabel('Channels');
ylabel('Intensities');

H = FU;
