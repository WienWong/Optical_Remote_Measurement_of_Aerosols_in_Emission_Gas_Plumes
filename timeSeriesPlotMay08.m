
% Time series plot on May08 (impressed chimney day) Tianjing 
% 2016-11-24, 1st built, Wang Weihua

clear;
format long; 
numRows=14716;                % #. of record STD files
numCols=2048;                 % #. of channels

% Read the 2ms dark spectrum
FD = csvread('/home/wien/Octave/flameDOAS/calibrations/dark/dark_2ms_1000av_160508.STD');
FDU = FD(4:2051,:);           % extract usefull rows
figure;
plot(FDU);
xlim([0,2050]);
title('2ms dark spectrum on May08'); 
xlabel('Channels');
ylabel('Intensities');

% Read the 14716 SDT files.
file = NaN(numRows,numCols);  % pre-locate a NaN matrix
dirpath='/home/wien/Octave/flameDOAS/spectra/160508/'

for kk=0:9
  filepath = [dirpath '085555/S000000' num2str(kk) '.STD'];
  fileorg = csvread(filepath);
  filecut = fileorg(4:2051,:)';
  file(kk+1,:) = filecut;
end

for ll=10:99
  filepath = [dirpath '085555/S00000' num2str(ll) '.STD'];
  fileorg = csvread(filepath);
  filecut = fileorg(4:2051,:)';
  file(ll+1,:) = filecut;
end

for mm=100:999
  filepath = [dirpath '085555/S0000' num2str(mm) '.STD'];
  fileorg = csvread(filepath);
  filecut = fileorg(4:2051,:)';
  file(mm+1,:) = filecut;
end

for nn=1000:9999
  filepath = [dirpath '085555/S000' num2str(nn) '.STD'];
  fileorg = csvread(filepath);
  filecut = fileorg(4:2051,:)';
  file(nn+1,:) = filecut;
end

for nn=10000:numRows-1
  filepath = [dirpath '085555/S00' num2str(nn) '.STD'];
  fileorg = csvread(filepath);
  filecut = fileorg(4:2051,:)';
  file(nn+1,:) = filecut;
end

% Just to check if all data are copied correctly.
file(1,1:5);
file(1,end);
file(10,1:5);
file(11,1:5);
file(11,end-3:end);
file(100,1:5);
file(100,end-3:end);
file(101,1:5);
file(101,end-3:end);
file(1000,1:5);       % Row of 999
file(1000,end-3:end);
file(1001,1:5);       % Row of 1000
file(1001,end-3:end);
file(10000,1:5);      % Row of 9999
file(10000,end-3:end);
file(10001,1:5);      % Row of 10000
file(10001,end-3:end);
file(end,1:5);
file(end,end-3:end);

%%%%%%%
save('spectra0508.dat', 'file','-ascii');
% To load this saved file
% file = load('spectra0508.dat');
%%%%%%%

% Try to plot the first 3 rows 
tt=1:2048*3;
yy=[file(1,:) file(2,:) file(3,:)];
% Save the plot in proper size/place by adjusting the figure properties.
figure;
set(gcf, "papersize", [6, 4], "paperposition", [0 0 6 4]); 
plot(tt,yy);

print test1.pdf;

% Try to plot the first 5 rows in a loop
tt=1:numCols*5;
yy=zeros(1,numCols*5);
for bb=1:5
  yy(:, [1:numCols]+numCols*(bb-1)) = file(bb,:);
end
figure;
set(gcf, "papersize", [6, 4], "paperposition", [0 0 6 4]); 
plot(tt,yy);

print test2.pdf;

% Time series plot
timestep=2/2048;
timeseries=0:timestep:(2*numRows-timestep); % same size as 1:numCols*numRows
ss = zeros(size(timeseries));

for vv=1:numRows      %14716
  ss(:, [1:numCols]+numCols*(vv-1)) = file(vv,:) - FDU'; %  - mn
end

figure;
set(gcf, "papersize", [6, 4], "paperposition", [0 0 6 4]); 
plot(timeseries/3600,ss);
xlim([0,2*numRows/3600]); 
xlabel('Time (hours)');
ylabel('Intensities');
title('Time Series Plot on May08 (Offset un-removed)');

%%%
% load the .dat file
%%%

% just to validate row by row concatenated
%ww=ones(1,20);
%xx=rand(4,5)*5;
%for rr=1:4
%ww(:, [1:5] + 5*(rr-1)) = xx(rr,:);
%end
%ww

%file = load('spectra0508.dat'); 

% Try to plot the first 5 measured spectrum - dark parts - further offset. 
numRows=14716;  % #. of record STD files
numCols=2048;   % #. of cahnnels

tt=1:numCols*5;
yy=zeros(1,numCols*5);
for bb=1:5
  yy(:, [1:numCols]+numCols*(bb-1)) = file(bb,:) - FDU';
end
figure;
set(gcf, "papersize", [6, 4], "paperposition", [0 0 6 4]); 
mn = mean(yy(:,60:110));
plot(tt,yy-mn);
title('First 5 measurement with offset removed');

% Plot whole day measured spectrum - dark parts - further offset. 
timestep=2/2048;
timeseries=0:timestep:(2*numRows-timestep); % same size as 1:numCols*numRows
ss = zeros(size(timeseries));  % Pre-locate memory

for vv=1:numRows      %14716
  ss(:, [1:numCols]+numCols*(vv-1)) = file(vv,:) - FDU' - mn;
end

figure;
%set(gcf, "papersize", [6, 4], "paperposition", [0 0 6 4]); 
plot(timeseries/3600,ss);
xlim([0,2*numRows/3600]); 
xlabel('Time (hours)');
ylabel('Intensities');
title('Time Series Plot on May08 (Offset removed)');
