
function F = caliSpectrl(File, Dark)
% This function performs spectra calibration 
% Measured spectrum - Dark spectrum - Further offset. 
% File: the whole day STD file loaded in advance.
% Dark: the dark spectrum loaded from 'darkspctrl.m' or 'readDarkHg.m' in advance. 
% Calibrated spectrum will be same size as input File.
% 2017-01-06 first built, Wang Weihua

fileN = zeros(size(File)); % Pre-locate 

for kk=1:size(File)(1)
  fileN(kk,:) = File(kk,:) - Dark' - mean( (File(kk,:) - Dark')(:,60:110) );
end

F = fileN;

endfunction
