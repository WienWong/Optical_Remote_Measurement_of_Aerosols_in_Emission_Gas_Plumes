function [D, Hg] = readDarkHg(dp, fn, numR, numC, dy, yue)

% Read for dark and Hg spectrum during May, June 2016 
% 2017-01-05 1st built, Weihua Wang
% dp: dirpath 
% fn: filename
% numR: number of rows
% numC: number of columns
% dy: day
% yue: month in Chinese

% Read the dark spectra SDT files.
file1 = NaN(numR(1),numC);   % pre-locate a NaN matrix

% fill in data for file1
if(0 <=numR(1) & numR(1) <= 9)
  for kk=0:numR(1)-1
    filepath = [dp fn(1:11) '/S000000' num2str(kk) '.STD'];
    fileorg = csvread(filepath);
    filecut = fileorg(4:2051,:)';
    file1(kk+1,:) = filecut;
  end
elseif(10 <=numR(1) & numR(1) <= 99)
  for kk=0:9
    filepath = [dp fn(1:11) '/S000000' num2str(kk) '.STD'];
    fileorg = csvread(filepath);
    filecut = fileorg(4:2051,:)';
    file1(kk+1,:) = filecut;
  end
  for ll=10:numR(1)-1
    filepath = [dp fn(1:11) '/S00000' num2str(ll) '.STD'];
    fileorg = csvread(filepath);
    filecut = fileorg(4:2051,:)';
    file1(ll+1,:) = filecut;
  end
elseif(100 <=numR(1) & numR(1) <= 999)
  for kk=0:9
    filepath = [dp fn(1:11) '/S000000' num2str(kk) '.STD'];
    fileorg = csvread(filepath);
    filecut = fileorg(4:2051,:)';
    file1(kk+1,:) = filecut;
  end
  for ll=10:99
    filepath = [dp fn(1:11) '/S00000' num2str(ll) '.STD'];
    fileorg = csvread(filepath);
    filecut = fileorg(4:2051,:)';
    file1(ll+1,:) = filecut;
  end
  for mm=100:numR(1)-1
    filepath = [dp fn(1:11) '/S0000' num2str(mm) '.STD'];
    fileorg = csvread(filepath);
    filecut = fileorg(4:2051,:)';
    file1(mm+1,:) = filecut;
  end
elseif(1000 <=numR(1) & numR(1) <= 9999)
  for kk=0:9
    filepath = [dp fn(1:11) '/S000000' num2str(kk) '.STD'];
    fileorg = csvread(filepath);
    filecut = fileorg(4:2051,:)';
    file1(kk+1,:) = filecut;
  end
  for ll=10:99
    filepath = [dp fn(1:11) '/S00000' num2str(ll) '.STD'];
    fileorg = csvread(filepath);
    filecut = fileorg(4:2051,:)';
    file1(ll+1,:) = filecut;
  end
  for mm=100:999
    filepath = [dp fn(1:11) '/S0000' num2str(mm) '.STD'];
    fileorg = csvread(filepath);
    filecut = fileorg(4:2051,:)';
    file1(mm+1,:) = filecut;
  end
    for mm=1000:numR(1)-1
    filepath = [dp fn(1:11) '/S000' num2str(mm) '.STD'];
    fileorg = csvread(filepath);
    filecut = fileorg(4:2051,:)';
    file1(mm+1,:) = filecut;
  end
endif

% mean dark spectrum plot
fil1 = mean(file1); % mean of all numR(1) rows 
figure;
plot(fil1);
xlim([0,2050]);
xlabel('Channels');
ylabel('Intensities');
if(yue==5)
  title([ 'Dark spectrum on May ' num2str(dy) ]);
elseif(yue==6)
  title([ 'Dark spectrum on June ' num2str(dy) ]);
endif

% Read the Hg spectra SDT files.
if(numR(2)==0)
  D = fil1;
  return;
endif

file2 = NaN(numR(2),numC);   % pre-locate a NaN matrix

% fill in data for file2, used for Hg spectra 
if(0 <=numR(2) & numR(2) <= 9)
  for kk=0:numR(2)-1
    filepath = [dp fn(12:20) '/S000000' num2str(kk) '.STD'];
    fileorg = csvread(filepath);
    filecut = fileorg(4:2051,:)';
    file2(kk+1,:) = filecut;
  end
elseif(10 <=numR(2) & numR(2) <= 99)
  for kk=0:9
    filepath = [dp fn(12:20) '/S000000' num2str(kk) '.STD'];
    fileorg = csvread(filepath);
    filecut = fileorg(4:2051,:)';
    file2(kk+1,:) = filecut;
  end
  for ll=10:numR(2)-1
    filepath = [dp fn(12:20) '/S00000' num2str(ll) '.STD'];
    fileorg = csvread(filepath);
    filecut = fileorg(4:2051,:)';
    file2(ll+1,:) = filecut;
  end
elseif(100 <=numR(2) & numR(2) <= 999)
  for kk=0:9
    filepath = [dp fn(12:20) '/S000000' num2str(kk) '.STD'];
    fileorg = csvread(filepath);
    filecut = fileorg(4:2051,:)';
    file2(kk+1,:) = filecut;
  end
  for ll=10:99
    filepath = [dp fn(12:20) '/S00000' num2str(ll) '.STD'];
    fileorg = csvread(filepath);
    filecut = fileorg(4:2051,:)';
    file2(ll+1,:) = filecut;
  end
  for mm=100:numR(2)-1
    filepath = [dp fn(12:20) '/S0000' num2str(mm) '.STD'];
    fileorg = csvread(filepath);
    filecut = fileorg(4:2051,:)';
    file2(mm+1,:) = filecut;
  end
elseif (1000 <=numR(2) & numR(2) <= 9999)
  for kk=0:9
    filepath = [dp fn(12:20) '/S000000' num2str(kk) '.STD'];
    fileorg = csvread(filepath);
    filecut = fileorg(4:2051,:)';
    file2(kk+1,:) = filecut;
  end
  for ll=10:99
    filepath = [dp fn(12:20) '/S00000' num2str(ll) '.STD'];
    fileorg = csvread(filepath);
    filecut = fileorg(4:2051,:)';
    file2(ll+1,:) = filecut;
  end
  for ll=100:999
    filepath = [dp fn(12:20) '/S0000' num2str(ll) '.STD'];
    fileorg = csvread(filepath);
    filecut = fileorg(4:2051,:)';
    file2(ll+1,:) = filecut;
  end
  for mm=1000:numR(2)-1
    filepath = [dp fn(12:20) '/S000' num2str(mm) '.STD'];
    fileorg = csvread(filepath);
    filecut = fileorg(4:2051,:)';
    file2(mm+1,:) = filecut;
  end
endif

% mean Hg spectrum plot
fil2 = mean(file2); % mean of all numR(2) rows 
figure;
plot(fil2);
xlim([0,2050]);
xlabel('Channels');
ylabel('Intensities');
if (yue==5)
  title([ 'Hg spectrum on May ' num2str(dy) ]);
elseif (yue==6)
  title([ 'Hg spectrum on June ' num2str(dy) ]);
endif

D = fil1;
Hg = fil2;

endfunction
