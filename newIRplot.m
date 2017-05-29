function newIRplot(tr,tv,IR,tottime,dy,yue=5,val=3000)

% Plot out the IR pairs and Intensity at 500 nm.
% 2017-01-05 1st built, Weihua Wang
% tr: timeratio
% tv: time vector for plot
% IR: calculated Intensity Ratio for selected pairs, a vector
% tottime: total time calculated by R code
% dy: specific day
% yue: month in Chinese Pinyin
% val: a value divided by the intensity for plotting rescale purpose.

figure;
plot(tr*tv,IR(:,1),tr*tv,IR(:,3),tr*tv,IR(:,4),tr*tv,IR(:,5),...
tr*tv,IR(:,7),tr*tv,IR(:,8),tr*tv,IR(:,10));   
xlim([0, tottime]); 
%ylim([-5, 10]);
legend('-ln(I_{310}/I_{340})','-ln(I_{340}/I_{500})','-ln(I_{340}/I_{675})',...
'-ln(I_{340}/I_{870})','-ln(I_{440}/I_{675})','-ln(I_{440}/I_{870})','-ln(I_{500}/I_{870})');
% legend('location','eastoutside');
legend('location','northwest');
xlabel('Time (seconds)');
ylabel('Ratio');
if(yue==6)
title(['Intensity ratio on June ' num2str(dy)],"fontsize", 15);
elseif(yue==5)
title(['Intensity ratio on May ' num2str(dy)],"fontsize", 15);
endif

% I500 and IR plot
figure;
plot(tr*tv,IR(:,end)/val);
hold on;
plot(tr*tv,IR(:,1),tr*tv,IR(:,3),tr*tv,IR(:,4),tr*tv,IR(:,5),...
tr*tv,IR(:,7),tr*tv,IR(:,8),tr*tv,IR(:,10));   
xlim([0, tottime]); 
ylim([-5, 25]);
xlabel('Time (seconds)');
legend(['I_{500} / ' num2str(val)], '-ln(I_{310}/I_{340})','-ln(I_{340}/I_{500})','-ln(I_{340}/I_{675})',...
'-ln(I_{340}/I_{870})','-ln(I_{440}/I_{675})','-ln(I_{440}/I_{870})','-ln(I_{500}/I_{870})');
legend('location','northeast');
%legend('location','eastoutside');
if(yue==5)
title(['Intensity Ratio and I_{500} on May ' num2str(dy)],"fontsize", 13);
elseif(yue==6)
title(['Intensity Ratio and I_{500} on June ' num2str(dy)],"fontsize", 13);
endif

% I500 plot
figure;
plot(tr*tv,IR(:,end));
xlim([0, tottime]);
xlabel('Time (seconds)');
ylabel('Intensity');
if(yue==5)
title(['I_{500} on May ' num2str(dy)],"fontsize", 15);
elseif(yue==6)
title(['I_{500} on June ' num2str(dy)],"fontsize", 15);
endif

endfunction
