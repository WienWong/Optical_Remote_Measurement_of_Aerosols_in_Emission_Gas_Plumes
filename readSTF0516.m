%%% 2016-12-19 first built by Wang Weihua
% Read and merge 'STD' files on May 16 2016

clear;
format long; 
numCols = 2048;                    % #. of channels

month = 5;
dy = 16;
numRows = [14710];                 % #. of record STD files on May 16
timevec = ["082208"];
timevec(1:6)

% Read the SDT files.
file1 = NaN(numRows(1),numCols);   % pre-locate a NaN matrix
%file2 = NaN(numRows(2),numCols);    

if (1<=dy & dy<=9)
  dirpath='/home/wien/Octave/flameDOAS/spectra/160';
  filpath= [dirpath num2str(month) '0' num2str(dy) '/']
elseif (10<= dy & dy<=31)
  dirpath='/home/wien/Octave/flameDOAS/spectra/160';
  filpath= [dirpath num2str(month) num2str(dy) '/']
endif

% fill in data for file1
for kk=0:9
  filepath = [filpath timevec(1:6) '/S000000' num2str(kk) '.STD'];
  fileorg = csvread(filepath);
  filecut = fileorg(4:2051,:)';
  file1(kk+1,:) = filecut;
end

for ll=10:99
  filepath = [filpath timevec(1:6) '/S00000' num2str(ll) '.STD'];
  fileorg = csvread(filepath);
  filecut = fileorg(4:2051,:)';
  file1(ll+1,:) = filecut;
end

for mm=100:999
  filepath = [filpath timevec(1:6) '/S0000' num2str(mm) '.STD'];
  fileorg = csvread(filepath);
  filecut = fileorg(4:2051,:)';
  file1(mm+1,:) = filecut;
end

for nn=1000:9999
  filepath = [filpath timevec(1:6) '/S000' num2str(nn) '.STD'];
  fileorg = csvread(filepath);
  filecut = fileorg(4:2051,:)';
  file1(nn+1,:) = filecut;
end

for nn=10000:numRows(1)-1 %  
  filepath = [filpath timevec(1:6) '/S00' num2str(nn) '.STD'];
  fileorg = csvread(filepath);
  filecut = fileorg(4:2051,:)';
  file1(nn+1,:) = filecut;
end

% Just to check if all data are copied correctly.
file1(1,1:5);
file1(1,end-4:end);
file1(10,1:5);
file1(11,1:5);
file1(11,end-4:end);
file1(100,1:5);
file1(100,end-4:end);
file1(101,1:5);
file1(101,end-4:end);
file1(1000,1:5);        % Row of 999
file1(1000,end-4:end);
file1(1001,1:5);        % Row of 1000
file1(1001,end-4:end);
file1(10000,1:5);       % Row of 9999
file1(10000,end-3:end);
file1(10001,1:5);       % Row of 10000
file1(10001,end-3:end);
file1(end,1:5);
file1(end,end-3:end);

% check if no NaN contained  
nanCheck(file1);

%%%%%%%
save('spectra0516.dat', 'file1','-ascii');

% save('spectra0516.mat', 'file','-ascii');

% To load this saved file
% file = load('spectra0516.dat');
%%%%%%%
