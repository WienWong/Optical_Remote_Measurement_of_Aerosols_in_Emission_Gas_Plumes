function I500noon = noonIntensiyplot(std_a, std_x, tr, IR, dy, mth)

% Plot the Intensity at 500 nm during noon time interval [11:45:00, 13:15:00].
% 2017-01-07 1st built, Wang Weihua
% std_a: the STD file at around 11:45:00
% std_x: the STD file at around 13:15:00
% tr   : time ratio 
% IR   : can be calculated from 'newIRcal.m'
% dy   : specific day involved
% mth  : month 

tv_noon  = std_a:std_x;
I_500 = IR(:,end);     % last column is for I500.
I500_noon = I_500(tv_noon,:);
  figure;
  plot(tr*tv_noon, I500_noon, '-m');
  xlabel('Seconds');
  ylabel('Intensity');
if (mth==5)
  title(['I_{500} at noon on May ' num2str(dy)]);
elseif (mth==6)
  title(['I_{500} at noon on June ' num2str(dy)]);
endif

I500noon = I500_noon;

endfunction
