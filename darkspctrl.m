
function D = darkspctrl(k)
% 2016-11-30, 1st built, Wang Weihua
% Plot out dark spectral of 2 ms exposure time for the specific measured day.
% eg: filepath='/home/wien/Octave/flameDOAS/calibrations/dark/dark_2ms_1000av_160508.STD';
% Available k is the date number of 6,7,8,10,11,13,15,16,17.

filepath=[];
dirpath='/home/wien/Octave/flameDOAS/calibrations/dark/';
if (1<=k && k<=9)
  filepath = [dirpath 'dark_2ms_1000av_16050' num2str(k) '.STD'];
elseif (10<=k && k<=31)
  filepath = [dirpath 'dark_2ms_1000av_1605' num2str(k) '.STD'];
else
  disp('k should between 1 and 31.');
end

FD = csvread(filepath); % read the 'SDT' file.
FDU = FD(4:2051,:);     % usefull information btw 4th row and 2051th row.
figure;
plot(FDU);
xlim([0,2050]);
title([ 'Dark spectrum on May ' num2str(k) ]);
xlabel('Channels');
ylabel('Intensities');

D = FDU;
