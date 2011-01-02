package com.kl.android.Alarming;

import java.io.IOException;
import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.media.AudioManager;
import android.media.MediaPlayer;
import android.net.Uri;
import android.os.Bundle;
import android.os.Vibrator;
import android.util.Log;
import android.view.Window;
import android.view.WindowManager;

public class AlarmAlert extends Activity {
	private MediaPlayer mMediaPlayer = null;
	private Vibrator mVibrator = null;
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		// hide the window title.
		requestWindowFeature(android.view.Window.FEATURE_NO_TITLE);
		turnScreenOn();
		// get index value.
		Bundle extras = getIntent().getExtras();
		if( extras == null ) return;
		Long index = extras.getLong("Index");
		long lindex = -1;
		if( index != null ) {
			lindex = index.longValue();
		}
		Log.d(ConfigData.LOGTAG, "AlarmAlert index " + lindex);
		ConfigData cfg = retriveCfg( (int) lindex);
		if( cfg == null ) return;
		notify( cfg );
		scheduleNext( cfg, (int)lindex );
	}
	
	private void turnScreenOn() {
        final Window win = getWindow();
        win.addFlags(WindowManager.LayoutParams.FLAG_SHOW_WHEN_LOCKED
                | WindowManager.LayoutParams.FLAG_DISMISS_KEYGUARD);
        win.addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON
                | WindowManager.LayoutParams.FLAG_TURN_SCREEN_ON);
	}
	
	private void scheduleNext( ConfigData cfg, int index ) {
		AlarmHandler.instance().schedule(cfg, this, index, true);
	}

	private ConfigData retriveCfg( int index ) {
		if( !Config.instance().isInited() ) {
			Config.instance().init(this);
		}
		ConfigData cfg = Config.instance().retriveCfg(index);
		return cfg;
	}
	
	private void notify( ConfigData cfg ) {
		playSound(cfg);
		vibrate(cfg);
		new AlertDialog.Builder(AlarmAlert.this).setIcon(R.drawable.icon)
		.setTitle("Alarm Clock")
		.setMessage("Alarm!" + cfg.mLabel)
		.setPositiveButton("Stop",
				new DialogInterface.OnClickListener() {
					public void onClick(DialogInterface dialog,
							int which) {
						if( mMediaPlayer != null ) {
							mMediaPlayer.stop();
							mMediaPlayer.release();
						}
						if( mVibrator != null ) {
							mVibrator.cancel();
						}
						AlarmAlert.this.finish();
					}
				})
		.show();		
	}
	
	private void playSound( ConfigData cfg ) {
		if(ConfigData.isNoRing(cfg.mRingID)) {
			Log.i(ConfigData.LOGTAG, "Ring is null.");
			return;
		}
		Log.i(ConfigData.LOGTAG, "Ring is: " + cfg.mRingID);
		mMediaPlayer = new MediaPlayer();
		try {
			mMediaPlayer.setDataSource(this, Uri.parse(cfg.mRingID));
			mMediaPlayer.setAudioStreamType(AudioManager.STREAM_ALARM);
			mMediaPlayer.prepare();
		} catch (IllegalStateException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		mMediaPlayer.setLooping(true);
		mMediaPlayer.start();
	}
	
	private void vibrate( ConfigData cfg ) {
		if( cfg.mWakeFlag ) {
			mVibrator = (Vibrator) getSystemService(VIBRATOR_SERVICE); 
			long[] pattern = {800, 50, 400, 30}; // OFF/ON/OFF/ON... 
			mVibrator.vibrate(pattern, 2);
		}
	}
}
