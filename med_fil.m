function datMF = med_fil(dat,kk)

%   dat: .dat file contains IRs and I500 to be median filtered
%    kk: median filtering width
% datMF: median filtered data

pkg load signal    % load spikes removing function 'medfilt1' which contained in 'signal' package

yy1 = dat(:,2);    % IR_340/440
yy2 = dat(:,3);    % IR_340/500
yy3 = dat(:,4);    % IR_340/675
yy4 = dat(:,6);    % IR_440/500
yy5 = dat(:,7);    % IR_440/675
yy6 = dat(:,12);   % I500 

fy1=medfilt1(yy1,kk);
fy2=medfilt1(yy2,kk);
fy3=medfilt1(yy3,kk);
fy4=medfilt1(yy4,kk);
fy5=medfilt1(yy5,kk);
fy6=medfilt1(yy6,kk);

datMF = [fy1, fy2, fy3, fy4, fy5, fy6];
