let peerConnection: any;
let streamId: string;
let sessionId: string;
let sessionClientAnswer: any;

let statsIntervalId: any;
let videoIsPlaying: boolean;
let lastBytesReceived: any;

let talkVideo: any;

const onIceGatheringStateChange = () => {
  console.log("iceGatheringState", peerConnection.iceGatheringState);
};

const onIceCandidate = (event: any) => {
  console.log("onIceCandidate", event);
  if (event.candidate) {
    const { candidate, sdpMid, sdpMLineIndex } = event.candidate;

    fetch(`https://api.d-id.com/talks/streams/${streamId}/ice`, {
      method: "POST",
      headers: {
        Authorization: `Basic ${process.env.NEXT_PUBLIC_D_ID_KEY}`,
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        candidate,
        sdpMid,
        sdpMLineIndex,
        session_id: sessionId,
      }),
    });
  }
};
function setVideoElement(stream: any) {
  if (!stream || !talkVideo) return;
  talkVideo.srcObject = stream;
  talkVideo.loop = false;

  // safari hotfix
  if (talkVideo.paused) {
    talkVideo
      .play()
      .then((_: any) => {})
      .catch((e: any) => {});
  }
}

function playIdleVideo() {
  // talkVideo.srcObject = undefined;
  // talkVideo.src = "or_idle.mp4";
  // talkVideo.loop = true;
}

function onVideoStatusChange(videoIsPlaying: boolean, stream: any) {
  let status;
  if (videoIsPlaying) {
    status = "streaming";
    const remoteStream = stream;
    setVideoElement(remoteStream);
  } else {
    status = "empty";
    playIdleVideo();
  }
  console.log("streaming status changed to", status);
}

function onTrack(event: any) {
  /**
   * The following code is designed to provide information about wether currently there is data
   * that's being streamed - It does so by periodically looking for changes in total stream data size
   *
   * This information in our case is used in order to show idle video while no talk is streaming.
   * To create this idle video use the POST https://api.d-id.com/talks endpoint with a silent audio file or a text script with only ssml breaks
   * https://docs.aws.amazon.com/polly/latest/dg/supportedtags.html#break-tag
   * for seamless results use `config.fluent: true` and provide the same configuration as the streaming video
   */

  if (!event.track) return;

  statsIntervalId = setInterval(async () => {
    const stats = await peerConnection.getStats(event.track);
    stats.forEach((report: any) => {
      if (report.type === "inbound-rtp" && report.mediaType === "video") {
        const videoStatusChanged =
          videoIsPlaying !== report.bytesReceived > lastBytesReceived;

        if (videoStatusChanged) {
          videoIsPlaying = report.bytesReceived > lastBytesReceived;
          onVideoStatusChange(videoIsPlaying, event.streams[0]);
        }
        lastBytesReceived = report.bytesReceived;
      }
    });
  }, 500);
}

const stopAllStreams = () => {
  if (talkVideo?.srcObject) {
    console.log("stopping video streams");
    (talkVideo.srcObject as any)
      .getTracks()
      .forEach((track: any) => track.stop());
    talkVideo.srcObject = null;
  }
};

const closePC = (pc = peerConnection) => {
  if (!pc) return;
  console.log("stopping peer connection");
  pc.close();
  pc.removeEventListener(
    "icegatheringstatechange",
    onIceGatheringStateChange,
    true
  );
  pc.removeEventListener("icecandidate", onIceCandidate, true);
  pc.removeEventListener(
    "iceconnectionstatechange",
    onIceConnectionStateChange,
    true
  );
  pc.removeEventListener(
    "connectionstatechange",
    onConnectionStateChange,
    true
  );
  pc.removeEventListener("signalingstatechange", onSignalingStateChange, true);
  pc.removeEventListener("track", onTrack, true);
  clearInterval(statsIntervalId);
  console.log("stopped peer connection");
  if (pc === peerConnection) {
    peerConnection = null;
  }
};

function onIceConnectionStateChange() {
  console.log("iceConnectionState", peerConnection?.iceConnectionState);
  if (
    peerConnection?.iceConnectionState === "failed" ||
    peerConnection?.iceConnectionState === "closed"
  ) {
    stopAllStreams();
    closePC();
  }
}

function onConnectionStateChange() {
  // not supported in firefox
  console.log("connectionState", peerConnection?.connectionState);
}
function onSignalingStateChange() {
  console.log("signalingState", peerConnection?.signalingState);
}

async function createPeerConnection(offer: any, iceServers: any) {
  if (!peerConnection) {
    peerConnection = new RTCPeerConnection({ iceServers });
    peerConnection.addEventListener(
      "icegatheringstatechange",
      onIceGatheringStateChange,
      true
    );
    peerConnection.addEventListener("icecandidate", onIceCandidate, true);
    peerConnection.addEventListener(
      "iceconnectionstatechange",
      onIceConnectionStateChange,
      true
    );
    peerConnection.addEventListener(
      "connectionstatechange",
      onConnectionStateChange,
      true
    );
    peerConnection.addEventListener(
      "signalingstatechange",
      onSignalingStateChange,
      true
    );
    peerConnection.addEventListener("track", onTrack, true);
  }

  await peerConnection.setRemoteDescription(offer);
  console.log("set remote sdp OK");

  const sessionClientAnswer = await peerConnection.createAnswer();
  console.log("create local sdp OK");

  await peerConnection.setLocalDescription(sessionClientAnswer);
  console.log("set local sdp OK");

  return sessionClientAnswer;
}

export const startSession = async (source_url: string, videoEl: any) => {
  if (peerConnection && peerConnection.connectionState === "connected") {
    return;
  }
  stopAllStreams();
  closePC();

  talkVideo = videoEl;

  const sessionResponse = await fetch(`https://api.d-id.com/talks/streams`, {
    method: "POST",
    headers: {
      Authorization: `Basic ${process.env.NEXT_PUBLIC_D_ID_KEY}`,
      "Content-Type": "application/json",
    },
    body: JSON.stringify({
      source_url,
    }),
  });
  console.log("sessionResponse", sessionResponse);

  const {
    id: newStreamId,
    offer,
    ice_servers: iceServers,
    session_id: newSessionId,
  } = await sessionResponse.json();
  streamId = newStreamId;
  sessionId = newSessionId;

  try {
    sessionClientAnswer = await createPeerConnection(offer, iceServers);

    const sdpResponse = await fetch(
      `https://api.d-id.com/talks/streams/${newStreamId}/sdp`,
      {
        method: "POST",
        headers: {
          Authorization: `Basic ${process.env.NEXT_PUBLIC_D_ID_KEY}`,
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          answer: sessionClientAnswer,
          session_id: sessionId,
        }),
      }
    );
    console.log("sdpResponse", sdpResponse);
  } catch (e) {
    console.log("error during streaming setup", e);
    stopAllStreams();
    closePC();
    return;
  }
};

export const speak = async (input: string, voice_id: string) => {
  if (
    peerConnection?.signalingState === "stable" ||
    peerConnection?.iceConnectionState === "connected"
  ) {
    const talkResponse = await fetch(
      `https://api.d-id.com/talks/streams/${streamId}`,
      {
        method: "POST",
        headers: {
          Authorization: `Basic ${process.env.NEXT_PUBLIC_D_ID_KEY}`,
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          script: {
            type: "text",
            subtitles: "false",
            provider: { type: "microsoft", voice_id },
            ssml: "false",
            input,
          },
          driver_url: "bank://lively/",
          config: {
            stitch: true,
          },
          session_id: sessionId,
        }),
      }
    );
    console.log("talkResponse", talkResponse);
  }
};
