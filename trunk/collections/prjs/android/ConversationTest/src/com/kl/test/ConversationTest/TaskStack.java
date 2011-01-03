package com.kl.test.ConversationTest;

import java.util.ArrayList;


public final class TaskStack {
	Thread mWorkerThread;
	private final ArrayList<Runnable> mThingsToLoad;

	public TaskStack() {
		mThingsToLoad = new ArrayList<Runnable>();
		mWorkerThread = new Thread(new Runnable() {
			public void run() {
				while (true) {
					Runnable r = null;
					synchronized (mThingsToLoad) {
						if (mThingsToLoad.size() == 0) {
							try {
								Log.d("Task empty, wait.");
								mThingsToLoad.wait();
							} catch (InterruptedException ex) {
								// nothing to do
							}
						}
						if (mThingsToLoad.size() > 0) {
							r = mThingsToLoad.remove(0);
						}
					}
					if (r != null) {
						Log.d("Run task.");
						r.run();
						Log.d("Task end.");
					}
				}
			}
		});
		mWorkerThread.start();
	}

	public void push(Runnable r) {
		synchronized (mThingsToLoad) {
			mThingsToLoad.add(r);
			mThingsToLoad.notify();
		}
	}
}
