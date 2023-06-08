
%% Read the image directory from the excel file
clear;
excelName = 'G:\My Drive\_Walsh lab\Data Results phasor\Data_Result_Phasor_07_16_BT';

writeExcelName = append(excelName,'.xlsx');
[~,txt] = xlsread(excelName,'Data Directory','B1:E63');
%% Generate NADH and FAD image(a1,t1,t2,intensity) directory list
[ImageNumber,~] = size(txt);

NADHa1ImageList = cell((ImageNumber - 1)/2,1);
NADHt1ImageList = cell((ImageNumber - 1)/2,1);
NADHt2ImageList = cell((ImageNumber - 1)/2,1);
NADHIntensityImageList = cell((ImageNumber - 1)/2,1);

imageMaskList = cell((ImageNumber - 1)/2,1);

FADa1ImageList = cell((ImageNumber - 1)/2,1);
FADt1ImageList = cell((ImageNumber - 1)/2,1);
FADt2ImageList = cell((ImageNumber - 1)/2,1);
FADIntensityImageList = cell((ImageNumber - 1)/2,1);

NADHSImageList = cell((ImageNumber - 1)/2,1);
NADHGImageList = cell((ImageNumber - 1)/2,1);
FADSImageList = cell((ImageNumber - 1)/2,1);
FADGImageList = cell((ImageNumber - 1)/2,1);

for i = 2:ImageNumber
    imageDirectory = char(txt(i,1));
    enzyme = char(txt(i,2));
    imageName = char(txt(i,3));
  
    if ~mod(i,2)
        
        j = i/2;
        NADHa1ImageList(j,1) = cellstr(Get_Image_Position(imageDirectory,enzyme,'a1[%]',imageName));
        NADHt1ImageList(j,1) = cellstr(Get_Image_Position(imageDirectory,enzyme,'t1',imageName));
        NADHt2ImageList(j,1) = cellstr(Get_Image_Position(imageDirectory,enzyme,'t2',imageName));
        NADHIntensityImageList(j,1) = cellstr(Get_Image_Position(imageDirectory,enzyme,'photons',imageName));
        imageMaskList(j, 1) = cellstr([imageDirectory, enzyme, '\', imageName,'_','photons_cell_mask','.tiff']);
        
        NADHGImageList(j,1) = cellstr(Get_Image_Position(imageDirectory,enzyme,'phasor_G',imageName));
        NADHSImageList(j,1) = cellstr(Get_Image_Position(imageDirectory,enzyme,'phasor_S',imageName));
        
    else
        j = (i-1)/2;
        FADa1ImageList(j,1) = cellstr(Get_Image_Position(imageDirectory,enzyme,'a1[%]',imageName));
        FADt1ImageList(j,1) = cellstr(Get_Image_Position(imageDirectory,enzyme,'t1',imageName));
        FADt2ImageList(j,1) = cellstr(Get_Image_Position(imageDirectory,enzyme,'t2',imageName));
        FADIntensityImageList(j,1) = cellstr(Get_Image_Position(imageDirectory,enzyme,'photons',imageName));
        
        FADGImageList(j,1) = cellstr(Get_Image_Position(imageDirectory,enzyme,'phasor_G',imageName));
        FADSImageList(j,1) = cellstr(Get_Image_Position(imageDirectory,enzyme,'phasor_S',imageName));
    end

end
rowNumber = 2;
sheetNameCopy = strings(1);
%% Calculate the lifetime components and phasor components
for l = 1:(ImageNumber - 1)/2
        
        % Read NADH and FAD lifetime images
        NADHa1Image = dlmread(char(NADHa1ImageList(l,1)));
        NADHt1Image = dlmread(char(NADHt1ImageList(l,1)));
        NADHt2Image = dlmread(char(NADHt2ImageList(l,1)));
        NADHIntensityImage = dlmread(char(NADHIntensityImageList(l,1)));

        FADa1Image = dlmread(char(FADa1ImageList(l,1)));
        FADt1Image = dlmread(char(FADt1ImageList(l,1)));
        FADt2Image = dlmread(char(FADt2ImageList(l,1)));
        FADIntensityImage = dlmread(char(FADIntensityImageList(l,1)));

        NADHGImage = dlmread(char(NADHGImageList(l,1)));
        NADHSImage = dlmread(char(NADHSImageList(l,1)));
        FADGImage = dlmread(char(FADGImageList(l,1)));
        FADSImage = dlmread(char(FADSImageList(l,1)));
        
        imageMask = imread(char(imageMaskList(l, 1)));
        if(size(imageMask,2)~= 256)
            
            imageMask = [imageMask zeros(size(imageMask,1),8)];
        end

        if(size(imageMask,1)~= 256)
            imageMask = [imageMask;zeros(3,size(imageMask,2))];
        end

        cellNumber =  unique(imageMask);
        [cellNumber,~] = size(cellNumber);
    
       % Define lifetime component list 
        NADHIntensityList = zeros(cellNumber-1,1);
        NADHa1List = zeros(cellNumber-1,1);
        NADHt1List = zeros(cellNumber-1,1);
        NADHt2List = zeros(cellNumber-1,1);
        NADHFLIMList = zeros(cellNumber-1,1);

        FADIntensityList = zeros(cellNumber-1,1);
        FADa1List = zeros(cellNumber-1,1);
        FADt1List = zeros(cellNumber-1,1);
        FADt2List = zeros(cellNumber-1,1);
        FADFLIMList = zeros(cellNumber-1,1);
        
        redoxRatioList = zeros(cellNumber-1,1);
        FLIRRList = zeros(cellNumber-1,1);
        
        NADHGList = zeros(cellNumber-1,1);
        NADHSList = zeros(cellNumber-1,1);
        FADGList = zeros(cellNumber-1,1);
        FADSList = zeros(cellNumber-1,1);
        
        % Generate FLIM endpoints list
        for i = 1: cellNumber-1
       
          
            
            imageMaskCopy = double(imageMask);            
            imageMaskCopy(imageMaskCopy~=i) = 0;
            imageMaskCopy = imageMaskCopy/i;
            
            cellNADHa1Image = NADHa1Image/100 .* imageMaskCopy;
            cellFADa1Image = FADa1Image/100 .* imageMaskCopy;
            
            cellFADa1Image(cellFADa1Image < 0.3)= 0;
            cellFADa1Image(cellFADa1Image > 1) = 0;
            cellNADHa1Image(cellNADHa1Image <0.3)=0;
            cellNADHa1Image(cellNADHa1Image > 1) = 0;
            imageMaskOne = cellFADa1Image;
            imageMaskOne(imageMaskOne>0) = 1;
            imageMaskTwo = cellNADHa1Image;
            imageMaskTwo(imageMaskTwo>0) = 1;
            imageMaskCopy = imageMaskOne.*imageMaskTwo;
            
            % Calculate FLIM endpoints
            cellNADHIntensityImage = NADHIntensityImage .* imageMaskCopy;
            cellNADHa1Image = NADHa1Image/100 .* imageMaskCopy;
            cellNADHt1Image = NADHt1Image .* imageMaskCopy;
            cellNADHt2Image = NADHt2Image .* imageMaskCopy;
            cellNADHFLIMImage = cellNADHa1Image.* cellNADHt1Image + (1 - cellNADHa1Image).* cellNADHt2Image;

            cellFADIntensityImage = FADIntensityImage .* imageMaskCopy;
            cellFADa1Image = FADa1Image/100 .* imageMaskCopy;
            cellFADt1Image = FADt1Image .* imageMaskCopy;
            cellFADt2Image = FADt2Image .* imageMaskCopy;
            cellFADFLIMImage = cellFADa1Image.* cellFADt1Image + (1 - cellFADa1Image).* cellFADt2Image;

            cellRedoxRatioImage = cellFADIntensityImage./(cellFADIntensityImage + cellNADHIntensityImage );
            cellFLIRRImage = (1 - cellNADHa1Image)./cellFADa1Image;
            cellFLIRRImage(isinf(cellFLIRRImage)) = NaN;
            
            
            cellNADHGImage = NADHGImage.* imageMaskCopy;
            cellNADHSImage = NADHSImage.* imageMaskCopy;
            cellFADGImage = FADGImage.* imageMaskCopy;
            cellFADSImage = FADSImage.* imageMaskCopy;
            
            %%
            cellNADHIntensity = nanmean(nonzeros(cellNADHIntensityImage));
            cellNADHa1 = nanmean(nonzeros(cellNADHa1Image));
            cellNADHt1 = nanmean(nonzeros(cellNADHt1Image));
            cellNADHt2 = nanmean(nonzeros(cellNADHt2Image));
            cellNADHFLIM = nanmean(nonzeros(cellNADHFLIMImage));

            cellFADIntensity = nanmean(nonzeros(cellFADIntensityImage));
            cellFADa1 = nanmean(nonzeros(cellFADa1Image));
            cellFADt1 = nanmean(nonzeros(cellFADt1Image));
            cellFADt2 = nanmean(nonzeros(cellFADt2Image));
            cellFADFLIM = nanmean(nonzeros(cellFADFLIMImage));

            cellRedoxRatio = nanmean(nonzeros(cellRedoxRatioImage));
            cellFLIRR = nanmean(nonzeros(cellFLIRRImage));

            
            cellNADHG = nanmean(nonzeros(cellNADHGImage));
            cellNADHS = nanmean(nonzeros(cellNADHSImage));
            cellFADG = nanmean(nonzeros(cellFADGImage));
            cellFADS = nanmean(nonzeros(cellFADSImage));
            %% Save features to excel
            
            if(isnan(cellNADHIntensity)||isnan(cellNADHa1)||isnan(cellNADHt1)||isnan(cellNADHt2)||isnan(cellNADHFLIM)||...
                    isnan(cellFADIntensity)||isnan(cellFADa1)||isnan(cellFADt1)||isnan(cellFADt2)||isnan(cellFADFLIM)||...
                    isnan(cellRedoxRatio)||isnan(cellFLIRR))

                NADHIntensityList(i,1) = NaN;
                NADHa1List(i,1) = NaN;
                NADHt1List(i,1) = NaN;
                NADHt2List(i,1) = NaN;
                NADHFLIMList(i,1) = NaN;

                FADIntensityList(i,1) = NaN;
                FADa1List(i,1) = NaN;
                FADt1List(i,1) = NaN;
                FADt2List(i,1) = NaN;
                FADFLIMList(i,1) = NaN;

                redoxRatioList(i,1) = NaN;
                FLIRRList(i,1) = NaN;
                
                NADHGList(i,1) = NaN;
                NADHSList(i,1) = NaN;
                FADGList(i,1) = NaN;
                FADSList(i,1) = NaN;

            else
                NADHIntensityList(i,1) = cellNADHIntensity;
                NADHa1List(i,1) = cellNADHa1;
                NADHt1List(i,1) = cellNADHt1;
                NADHt2List(i,1) = cellNADHt2;
                NADHFLIMList(i,1) = cellNADHFLIM;

                FADIntensityList(i,1) = cellFADIntensity;
                FADa1List(i,1) = cellFADa1;
                FADt1List(i,1) = cellFADt1;
                FADt2List(i,1) = cellFADt2;
                FADFLIMList(i,1) = cellFADFLIM;

                redoxRatioList(i,1) = cellRedoxRatio;
                FLIRRList(i,1) = cellFLIRR;
                
                NADHGList(i,1) = cellNADHG;
                NADHSList(i,1) = cellNADHS;
                FADGList(i,1) = cellFADG;
                FADSList(i,1) = cellFADS;
            end   

        end
        
        NADHImageSaveName = char(txt(l*2,3));
        FADImageSaveName = char(txt(l*2+1,3));
        sheetName = string(txt(l*2,4));
        if ~strcmp(sheetName,sheetNameCopy)
            rowNumber = 2;
        end 
        
        
        
        xlswrite(writeExcelName,{NADHImageSaveName},sheetName,['A',int2str(rowNumber),':','A',int2str(rowNumber)])
        xlswrite(writeExcelName,{FADImageSaveName},sheetName,['A',int2str(rowNumber+1),':','A',int2str(rowNumber+1)])
        
        xlswrite(writeExcelName,NADHIntensityList,sheetName,['B',int2str(rowNumber),':','B',int2str(rowNumber + cellNumber - 2)])
        xlswrite(writeExcelName,NADHa1List,sheetName,['C',int2str(rowNumber),':','C',int2str(rowNumber + cellNumber - 2)])
        xlswrite(writeExcelName,NADHt1List,sheetName,['D',int2str(rowNumber),':','D',int2str(rowNumber + cellNumber - 2)])
        xlswrite(writeExcelName,NADHt2List,sheetName,['E',int2str(rowNumber),':','E',int2str(rowNumber + cellNumber - 2)])
        xlswrite(writeExcelName,NADHFLIMList,sheetName,['F',int2str(rowNumber),':','F',int2str(rowNumber + cellNumber - 2)])
        
        xlswrite(writeExcelName,FADIntensityList,sheetName,['G',int2str(rowNumber),':','G',int2str(rowNumber + cellNumber - 2)])
        xlswrite(writeExcelName,FADa1List,sheetName,['H',int2str(rowNumber),':','H',int2str(rowNumber + cellNumber - 2)])
        xlswrite(writeExcelName,FADt1List,sheetName,['I',int2str(rowNumber),':','I',int2str(rowNumber + cellNumber - 2)])
        xlswrite(writeExcelName,FADt2List,sheetName,['J',int2str(rowNumber),':','J',int2str(rowNumber + cellNumber - 2)])
        xlswrite(writeExcelName,FADFLIMList,sheetName,['K',int2str(rowNumber),':','K',int2str(rowNumber + cellNumber - 2)])
        
        xlswrite(writeExcelName,redoxRatioList,sheetName,['L',int2str(rowNumber),':','L',int2str(rowNumber + cellNumber - 2)])        
        xlswrite(writeExcelName,FLIRRList,sheetName,['M',int2str(rowNumber),':','M',int2str(rowNumber + cellNumber - 2)])
        
        xlswrite(writeExcelName,NADHGList,sheetName,['N',int2str(rowNumber),':','N',int2str(rowNumber + cellNumber - 2)])        
        xlswrite(writeExcelName,NADHSList,sheetName,['O',int2str(rowNumber),':','O',int2str(rowNumber + cellNumber - 2)])
        xlswrite(writeExcelName,FADGList,sheetName,['P',int2str(rowNumber),':','P',int2str(rowNumber + cellNumber - 2)])        
        xlswrite(writeExcelName,FADSList,sheetName,['Q',int2str(rowNumber),':','Q',int2str(rowNumber + cellNumber - 2)])

        sheetNameCopy = sheetName;
        rowNumber = rowNumber + cellNumber;
end


%% Phasor plot of a typical NADH image
l = 30;
NADHa1Image = dlmread(char(NADHa1ImageList(l,1)));
NADHt1Image = dlmread(char(NADHt1ImageList(l,1)));
NADHt2Image = dlmread(char(NADHt2ImageList(l,1)));
NADHIntensityImage = dlmread(char(NADHIntensityImageList(l,1)));

NADHGImage = dlmread(char(NADHGImageList(l,1)));
NADHSImage = dlmread(char(NADHSImageList(l,1)));

% Draw G/S phasor plot
imageMask = imread(char(imageMaskList(l, 1)));
if(size(imageMask,2)~= 256)
    imageMask = [imageMask zeros(size(imageMask,1),8)];
end

if(size(imageMask,1)~= 256)
    imageMask = [imageMask;zeros(3,size(imageMask,2))];
end

cellNumber =  unique(imageMask);
[cellNumber,~] = size(cellNumber);


imageMaskCopy = double(imageMask);
imageMaskCopy(imageMaskCopy~=0)=1;
NADHGImage = imageMaskCopy .* NADHGImage;
NADHSImage = imageMaskCopy .* NADHSImage;
NADHTmImage = (NADHa1Image.*NADHt1Image + (100-NADHa1Image).*NADHt2Image)./100;
NADHTmImage = NADHTmImage .* imageMaskCopy;
% Filter zero point
NADHTmVector = NADHTmImage(:);
NADHGVector = NADHGImage(:);
NADHSVector = NADHSImage(:);
zeroIndex =  NADHGVector==0;
NADHGVector(zeroIndex) = [];
NADHSVector(zeroIndex) = [];
NADHTmVector (zeroIndex) = [];

[sortedNADHtm, sortIndexes] = sort(NADHTmVector);
xs = NADHGVector(sortIndexes);
ys = NADHSVector(sortIndexes);
cmap = jet(length(NADHTmVector));
colormap(cmap)

%%
% draw NADH free-bound line
repetitionRate = 12.5 * 1e-9;
w = 2*pi*1/repetitionRate;
t1 = 3* 1e-9;
g1 = 1/(1 + ((w*t1)^2));
s1 = (w*t1)/(1 + ((w*t1)^2));

t2 = 0.5* 1e-9;
g2 = 1/(1 + (w*t2)^2);
s2 = (w*t2)/(1 + (w*t2)^2);

% Draw semi-circle
subplot(1,2,1)
x=0.5;      
y=0;
r=0.5;
theta = linspace(-2*pi/2,-4*pi/2, 100);  % <-- left half of circle
xCirc = r * cos(theta) + x;
yCirc = r * sin(theta) + y;
cla()
plot(xCirc, yCirc)
axis equal
xlim([0, 1]);
ylim([0, 0.5]);

grid off
xline(x)
yline(y)
% Draw density figure
hold on;
% scatter(xs, ys, 10, cmap, 'filled')
scatter_kde(xs, ys, 'filled', 'MarkerSize', 10);
cb = colorbar();
cb.Label.String = 'Probability density estimate';
% Draw line
hold on;
plot([g1 g2], [s1 s2])

subplot(1,2,2)

NADHTmImage(NADHTmImage ==0)= NaN;
imagesc(NADHTmImage,'AlphaData', ~isnan(NADHTmImage));
colormap(cmap);
caxis([400 1800]);
axis image
% Make a black axis
set(gca, 'XColor', 'none', 'yColor', 'none', 'xtick', [], 'ytick', [], 'Color', 'black')
%% Phasor plot of a typical FAD image
figure();

FADa1Image = dlmread(char(FADa1ImageList(l,1)));
FADt1Image = dlmread(char(FADt1ImageList(l,1)));
FADt2Image = dlmread(char(FADt2ImageList(l,1)));
FADIntensityImage = dlmread(char(FADIntensityImageList(l,1)));

FADGImage = dlmread(char(FADGImageList(l,1)));
FDASImage = dlmread(char(FADSImageList(l,1)));

% Draw G/S phasor plot
imageMask = imread(char(imageMaskList(l, 1)));
if(size(imageMask,2)~= 256)
    imageMask = [imageMask zeros(size(imageMask,1),8)];
end

if(size(imageMask,1)~= 256)
    imageMask = [imageMask;zeros(3,size(imageMask,2))];
end

cellNumber =  unique(imageMask);
[cellNumber,~] = size(cellNumber);


imageMaskCopy = double(imageMask);
imageMaskCopy(imageMaskCopy~=0)=1;
FADGImage = imageMaskCopy .* FADGImage;
FDASImage = imageMaskCopy .* FDASImage;
FADTmImage = (FADa1Image.*FADt1Image + (100-FADa1Image).*FADt2Image)./100;
FADTmImage = FADTmImage .* imageMaskCopy;

% Filter zero point
FADTmVector = FADTmImage(:);
FADGVector = FADGImage(:);
FADSVector = FDASImage(:);
zeroIndex =  FADGVector==0;
FADGVector(zeroIndex) = [];
FADSVector(zeroIndex) = [];
FADTmVector (zeroIndex) = [];

[sortedFADtm, sortIndexes] = sort(FADTmVector);
xs = FADGVector(sortIndexes);
ys = FADSVector(sortIndexes);

% cmap = jet(length(FADTmVector));
% colormap(cmap)
%%
% draw FAD free-bound line
repetitionRate = 12.5 * 1e-9;
w = 2*pi*1/repetitionRate;
t1 = 3.2* 1e-9;
g1 = 1/(1 + ((w*t1)^2));
s1 = (w*t1)/(1 + ((w*t1)^2));

t2 = 0.9* 1e-9;
g2 = 1/(1 + (w*t2)^2);
s2 = (w*t2)/(1 + (w*t2)^2);

% Draw semi-circle
%subplot(1,2,1)
x=0.5;      
y=0;
r=0.5;
theta = linspace(-2*pi/2,-4*pi/2, 100);  % <-- left half of circle
xCirc = r * cos(theta) + x;
yCirc = r * sin(theta) + y;
cla()
plot(xCirc, yCirc)
axis equal
xlim([0, 1]);
ylim([0, 0.5]);

grid off
xline(x)
yline(y)
% Draw density figure
hold on;
% scatter(xs, ys, 10, cmap, 'filled')
scatter_kde(xs, ys, 'filled', 'MarkerSize', 10);
cb = colorbar();
cb.Label.String = 'Probability density estimate';
% Draw line
hold on;
plot([g1 g2], [s1 s2])
%%
figure();
%subplot(1,2,2)
colormap(cmap);
FADTmImage(FADTmImage ==0)= NaN;
imagesc(FADTmImage,'AlphaData', ~isnan(FADTmImage));
cb = colorbar();
caxis([0 1500]);
axis image
% Make a black axis
set(gca, 'XColor', 'none', 'yColor', 'none', 'xtick', [], 'ytick', [], 'Color', 'black')
filename = "FAD_Tm_"+ imageName+".tiff";
fig = get_param('FADTmImage','Handle')
saveas(fig,filename)
%%
function imagePosition =  Get_Image_Position(imageDirectory,enzyme,flimFeature,imageName)
imagePosition = [imageDirectory, enzyme, '\', imageName,'_',flimFeature,'.asc'];
end


function h = scatter_kde(x, y, varargin)
% Scatter plot where each point is colored by the spatial density of nearby
% points. The function use the kernel smoothing function to compute the
% probability density estimate (PDE) for each point. It uses the PDE has
% color for each point.
%
% Input
%     x <Nx1 double> position of markers on X axis
%     y <Nx1 double> posiiton of markers on Y axis
%     varargin can be used to send a set of instructions to the scatter function
%           Supports the MarkerSize parameter
%           Does not support the MarkerColor parameter
%
% Output:
%     h returns handles to the scatter objects created
%
% Example
%     % Generate data
%     x = normrnd(10,1,1000,1);
%     y = x*3 + normrnd(10,1,1000,1);
%     % Plot data using probability density estimate as function
%     figure(1); 
%     scatter_kde(x, y, 'filled', 'MarkerSize', 100);
%     % Add Color bar
%     cb = colorbar();
%     cb.Label.String = 'Probability density estimate';
%
% author: Nils Haentjens
% created: Jan 15, 2018
% Use Kernel smoothing function to get the probability density estimate (c)
c = ksdensity([x,y], [x,y]);
if nargin > 2
  % Set Marker Size
  i = find(strcmp(varargin, 'MarkerSize'),1);
  if ~isempty(i); MarkerSize = varargin{i+1}; varargin(i:i+1) = [];
  else MarkerSize = []; end
  % Plot scatter plot
  h = scatter(x, y, MarkerSize, c, varargin{:});
else
  h = scatter(x, y, [], c);
end
end

% %% Test Code
% NADHGVector = NADHGImage(:);
% NADHSVector = NADHSImage(:);
% 
% % Filter zero point
% zeroIndex =  NADHGVector==0;
% NADHGVector(zeroIndex) = [];
% NADHSVector(zeroIndex) = [];
% 
% repetitionRate = 12.5 * 1e-9;
% w = 2*pi*1/repetitionRate;
% t1 = 3* 1e-9;
% g1 = 1/(1 + ((w*t1)^2));
% s1 = (w*t1)/(1 + ((w*t1)^2));
% 
% t2 = 0.5* 1e-9;
% g2 = 1/(1 + (w*t2)^2);
% s2 = (w*t2)/(1 + (w*t2)^2);
% % draw semi circle
% x=0.5;      
% y=0;
% r=0.5;
% theta = linspace(-2*pi/2,-4*pi/2, 100);  % <-- left half of circle
% xCirc = r * cos(theta) + x;
% yCirc = r * sin(theta) + y;
% cla()
% plot(xCirc, yCirc)
% axis equal
% grid off
% xline(x)
% yline(y)
% 
% %
% hold on;
% sz = 10;
% scatter(NADHGVector,NADHSVector,sz,'filled')
% 
% %
% hold on;
% plot([g1 g2], [s1 s2])
