% Notes: After training the neural network, run "output = net(testX)" to
% get the results of the test set. And then use function convert_output to
% convert the results to binary values.


% clear all
clc

% FormatAudioInput_same_pos

% global xBot

%% Set up num of inputs and num of layers

net = network;
net.numInputs = 3;  % Number of Inputs. Already been set to 3.
net.numLayers = 3;

%% Layer connection. This part does not need to be changed.

net.biasConnect = [ 1 ; 1 ; 1 ];
net.inputConnect(1, 1) = 1;
net.inputConnect(1, 2) = 1;
net.inputConnect(1, 3) = 1;

net.layerConnect(2, 1) = 1;
net.layerConnect(3, 2) = 1;

% net.layerConnect(4, 3) = 1;
% net.layerConnect(5, 4) = 1;

net.outputConnect = [0 0 1];

%% Dimentions of inputs. Change this base on what out inputs look like.

net.inputs{1}.exampleInput = zeros(64, 1);   % "2" should be changed to the dimention of our input
net.inputs{2}.exampleInput = zeros(240000, 1);
net.inputs{3}.exampleInput = zeros(240000, 1);

%% Parameters of Hidden Layers. This part does not need to be changed.
net.layers{1}.size = 15;
net.layers{1}.transferFcn = 'tansig';
net.layers{1}.initFcn = 'initnw';
net.layers{1}.netInputFcn = 'netsum';

net.layers{2}.size = 15;
net.layers{2}.transferFcn = 'tansig';
net.layers{2}.initFcn = 'initnw';
net.layers{2}.netInputFcn = 'netsum';
 
% net.layers{3}.size = 30;
% net.layers{3}.transferFcn = 'logsig';
% net.layers{3}.initFcn = 'initnw';
% net.layers{3}.netInputFcn = 'netsum';

% net.layers{4}.size = 20;
% net.layers{4}.transferFcn = 'tansig';
% net.layers{4}.initFcn = 'initnw';

%% Parameters of Output Layer. Change this part to specify what should the outputs look like.

net.layers{3}.size = 2; % the value on right-hand side should be equal to the dimention of outputs
net.layers{3}.transferFcn = 'softmax';
net.layers{3}.initFcn = 'initnw';

net.outputs{3}.exampleOutput = [ 0 ; 1 ]; % right-hand side is for telling Matlab the dimension of outputs

%% Set up trainning parameters. This part does not need to be changed.

net.trainFcn = 'trainscg';

net.performFcn = 'crossentropy';

net.divideFcn = 'dividerand';
net.initFcn = 'initlay';
net.plotFcns = {'plotperform','plottrainstate','plotconfusion','plotroc'};
%,'ploterrcorr','ploterrhist','plotfit','plotinerrcorr'};

% net.input.processFcns = {'mapminmax'};
% net.output.processFcns = {'mapminmax'};

net = init(net);

%% Format inputs
Input = cell(3,1);
Input{1} = inputbty;
Input{2} = inputaudio;
Input{3} = fftinput;

Target = zeros(2, 200);
Target(1, 1:100) = 1;
Target(2, 101:200) = 1;
% 
% BotBTY = '*';   % flag to read from bty file
% depthB = inf;
% rBox   = inf;
% 
% for k=1: 100;
%     infilename = ;
%     inbtyfile = ['bump_', num2str(k)];
%     outbtyfile = [''];
%     readbty(inbtyfile,BotBTY,depthB,rBox);
%     Input{1} = xBot(2,:);
%     [Input{2}, t] = audioread(strcat(infilename,'_rts_Rd_1_Rr_1.wav'));
%     Input{3} = abs(fft(Input{2}));
%     readbty(outbtyfile,BotBTY,depthB,rBox);
%     Target = xBot(2,:); % Load the target file(ground truth)
% end

trainingX = Input;    % Separated loaded file into training set and test set. This is traning set.
trainingT = Target;

% testX = Input{:, 801:1000}; % This is to reserve a part of the input for testing purposes.
% testT = Target{:, 801:1000};


%% Train
net = train(net, trainingX, trainingT);



