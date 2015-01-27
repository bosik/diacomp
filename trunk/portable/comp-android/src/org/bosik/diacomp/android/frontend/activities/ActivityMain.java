package org.bosik.diacomp.android.frontend.activities;

import java.util.Date;
import org.bosik.diacomp.android.BuildConfig;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.Storage;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.android.utils.ErrorHandler;
import android.app.Activity;
import android.app.ProgressDialog;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;

public class ActivityMain extends Activity
{
	/* =========================== CONSTANTS ================================ */

	static final String	TAG	= ActivityMain.class.getSimpleName();
	// private static final int RESULT_SPEECH_TO_TEXT = 620;

	/* =========================== FIELDS ================================ */

	// Components
	private Button		buttonDiary;
	private Button		buttonBase;

	/* =========================== CLASSES ================================ */

	// TODO: вынести в отдельный модуль, отвязать
	// TODO: refresh diary view on productive (non-trivial) sync

	class AsyncTaskAnalyzeDiary extends AsyncTask<Void, Integer, Void>
	{
		// <Params, Progress, Result>
		private ProgressDialog	dialog;

		@Override
		protected void onPreExecute()
		{
			dialog = new ProgressDialog(ActivityMain.this);
			dialog.setProgressStyle(ProgressDialog.STYLE_HORIZONTAL);
			dialog.setCancelable(false);
			// TODO: localization
			dialog.setMessage("Analyzing...");
		}

		@Override
		protected Void doInBackground(Void... par)
		{
			publishProgress(50);
			Storage.analyzeKoofs();
			publishProgress(100);

			return null;
		}

		@Override
		protected void onPostExecute(Void result)
		{
			if (dialog.isShowing())
			{
				dialog.dismiss();
			}

			// TODO: i18n
			UIUtils.showTip(ActivityMain.this, "Model updated");
		}
	}

	/* =========================== METHODS ================================ */

	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		try
		{
			if (BuildConfig.DEBUG)
			{
				UIUtils.showTip(this, "Debug mode is on");
			}

			// Core initialization
			ActivityPreferences.init(this);
			Storage.init(this, getContentResolver(), ActivityPreferences.preferences);

			// НАСТРОЙКА ИНТЕРФЕЙСА

			// устанавливаем макет
			setContentView(R.layout.activity_main);

			// определяем компоненты
			buttonDiary = (Button) findViewById(R.id.ButtonDiary);
			buttonBase = (Button) findViewById(R.id.ButtonBase);

			// назначаем обработчики
			buttonDiary.setOnClickListener(new OnClickListener()
			{
				@Override
				public void onClick(View v)
				{
					showDiary();
				}
			});
			buttonBase.setOnClickListener(new OnClickListener()
			{
				@Override
				public void onClick(View v)
				{
					Intent intent = new Intent(ActivityMain.this, ActivityBase.class);
					startActivity(intent);
				}
			});

			showDiary();
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}
	}

	void showDiary()
	{
		Intent intent = new Intent(this, ActivityDiary.class);
		intent.putExtra(ActivityDiary.KEY_DATE, new Date());
		startActivity(intent);
	}
}