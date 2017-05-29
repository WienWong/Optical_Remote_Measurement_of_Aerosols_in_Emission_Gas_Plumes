
function F = spctrladjust0508(num)

% 2016-11-30, 1st built, Wang Weihua
% Load the original measured spectrum, subtract the dark spectrum, and further 
% adjust by subtract by an avg value estimated from channel 50 to channel 100.
% Test spectrum from 'spectra->160508->085555->S0000477.STD'

numRows=14716;    % #. of record STD files
numCols=2048;     % #. of channels

filepath=[];
dirpath='/home/wien/Octave/flameDOAS/spectra/160508/';

if (0<=num && num<=9)
  filepath = [dirpath '085555/S000000' num2str(num) '.STD'];
elseif (10<=num && num<=99)
  filepath = [dirpath '085555/S00000' num2str(num) '.STD'];
elseif (100<=num && num<=999)
  filepath = [dirpath '085555/S0000' num2str(num) '.STD'];
elseif (1000<=num && num<=9999)
  filepath = [dirpath '085555/S000' num2str(num) '.STD'];
elseif (10000<=num && num<=numRows-1)
  filepath = [dirpath '085555/S00' num2str(num) '.STD'];
end

FS = csvread(filepath);
FSU = FS(4:2051,:);
plot(FSU);
xlim([0,2050]);
xlabel('Channels');
ylabel('Intensities');
title('Original spectrum on May 08');

% Subtract from the dark term
FDU = darkspctrl(8);
FSM = FSU-FDU;       % M means middle not the final result
figure;
plot(FSM);
xlim([0,2050]);
xlabel('Channels');
ylabel('Intensities');
title('Original spectrum - Dark spectrum');

% Still some offset not removed (may above or below 0).
% Futher to remove the offset by averaging a selected channel interval.
size(FSM(50:100,:));
mn = mean(FSM(50:100,:));

%% Spectrum with offset term removed. 
FSF = FSM-mn;  % F means final result
figure;
plot(FSF);
xlim([0,2050]);
title('Final adjusted spectrum');
xlabel('Channels');
ylabel('Intensities');


F = FSF;
