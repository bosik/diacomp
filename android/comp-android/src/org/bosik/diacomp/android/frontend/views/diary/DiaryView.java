package org.bosik.diacomp.android.frontend.views.diary;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.business.diary.records.NoteRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Paint.Style;
import android.graphics.Point;
import android.graphics.RectF;
import android.graphics.Typeface;
import android.util.AttributeSet;
import android.util.Log;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup.LayoutParams;
import android.view.WindowManager;

public class DiaryView extends View
{
	// отладочная печать
	static final String						TAG						= DiaryView.class.getSimpleName();

	// стили рисования
	private static final Paint				paintNoPage				= new Paint();
	private static final Paint				paintCaption			= new Paint();
	private static final Paint				paintTime				= new Paint();
	private static final Paint				paintRec				= new Paint();
	private static final Paint				paintDefault			= new Paint();

	private static final SimpleDateFormat	FORMAT_DIARY_TIME_LOC	= new SimpleDateFormat("HH:mm", Locale.US);

	// отступы
	private static final int				BORD					= 24;
	private static final int				TEXT_SIZE				= 48;
	private static final int				TEXT_NOPAGE_SIZE		= 64;
	private static final int				TEXT_BORD				= 20;
	private static int						LEFT_TIME;
	private static int						LEFT_RECS;
	static final int						REC_HEIGHT;

	// цвета
	private static final int				COLOR_PANEL_LIGHT_BORD	= Color.WHITE;
	private static final int				COLOR_PANEL_DARK_BORD	= Color.GRAY;
	private static final int				COLOR_PANEL_BLOOD_STD	= Color.rgb(230, 238, 255);
	private static final int				COLOR_PANEL_BLOOD_SEL	= Color.rgb(204, 221, 247);
	private static final int				COLOR_PANEL_INS_STD		= Color.WHITE;
	private static final int				COLOR_PANEL_INS_SEL		= Color.rgb(240, 240, 240);
	private static final int				COLOR_PANEL_NOTE_STD	= Color.rgb(216, 255, 228);
	private static final int				COLOR_PANEL_NOTE_SEL	= Color.rgb(179, 255, 202);
	private static final int				COLOR_PANEL_MEAL_STD	= Color.rgb(255, 255, 221);
	private static final int				COLOR_PANEL_MEAL_SEL	= Color.rgb(255, 255, 153);
	private static final int				COLOR_BACKGROUND		= Color.WHITE;

	// поля

	// private static final String TEXT_NOPAGE = "Страница пуста";
	private String[]						fingers;
	private int								screenWidth				= getScreenWidth();
	List<Versioned<DiaryRecord>>			records					= null;
	private Bitmap							bufferBitmap;
	private Canvas							bufferCanvas;
	int										clickedX				= -1;
	int										clickedY				= -1;
	private static int						downedIndex				= -1;
	private static int						clickedIndex			= -1;
	RecordClickListener						recordClickListener;

	// инициализация
	static
	{
		// before first use of timeToStr()
		FORMAT_DIARY_TIME_LOC.setTimeZone(TimeZone.getDefault());

		paintNoPage.setColor(Color.GRAY);
		paintNoPage.setTextSize(TEXT_NOPAGE_SIZE);
		paintNoPage.setAntiAlias(true);

		paintCaption.setColor(Color.GRAY);
		paintCaption.setTextSize((TEXT_SIZE * 3) / 4);
		paintCaption.setAntiAlias(true);

		paintTime.setColor(Color.BLACK);
		paintTime.setTextSize(TEXT_SIZE);
		paintTime.setAntiAlias(true);
		paintTime.setTypeface(Typeface.DEFAULT_BOLD);

		paintRec.setColor(Color.BLACK);
		paintRec.setTextSize(TEXT_SIZE);
		paintRec.setAntiAlias(true);

		LEFT_TIME = BORD + TEXT_BORD;
		LEFT_RECS = BORD + (2 * TEXT_BORD) + getTextWidth(timeToStr(new Date()), paintTime);
		REC_HEIGHT = TEXT_SIZE + (2 * TEXT_BORD);
	}

	/* ================================================================================== */

	// внешние методы
	public DiaryView(Context context, AttributeSet attrs)
	{
		super(context, attrs);
		// if (!isInEditMode()) setMinimumHeight(300);

		if (!isInEditMode())
		{
			fingers = getResources().getStringArray(R.array.fingers_short);
		}
		setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				Log.e(TAG, "onClick()");

				setClickedIndex(getDownedIndex());
				updateBuffer();
				invalidate();

				if ((records != null) && (getDownedIndex() >= 0) && (getDownedIndex() < records.size()))
				{
					if (recordClickListener != null)
					{
						recordClickListener.onRecordClick(getDownedIndex());
					}
				}
			}
		});
		setOnTouchListener(new OnTouchListener()
		{
			@Override
			public boolean onTouch(View v, MotionEvent event)
			{
				if (event.getAction() == MotionEvent.ACTION_DOWN)
				{
					clickedX = (int) event.getX();
					clickedY = (int) event.getY();
					setDownedIndex((clickedY - BORD) / REC_HEIGHT);
				}
				return false;
			}
		});
		setClickable(true);
	}

	/**
	 * Устанавливает страницу для просмотра и редактирования
	 * 
	 * @param page
	 *            Страница
	 */
	public void setRecords(List<Versioned<DiaryRecord>> records)
	{
		// Log.i(TAG, "setPage()");
		if (null == records)
		{
			throw new NullPointerException("Records list can't be null");
		}

		setDownedIndex(-1);
		setClickedIndex(-1);

		/**
		 * Save list to persist it through the view recreations (f.e. when screen is re-oriented)
		 */
		this.records = records;
		setMeasuredDimension(getScreenWidth(), getPageHeight(records));

		updateBuffer();
		invalidate();
		setMinimumHeight(getPageHeight(records));
		// Log.d(TAG, "setPage(): dimensions are setted");
		// setMeasuredDimension(screenWidth(), getPageHeight(page));
	}

	// =========================== GET / SET ===========================

	public RecordClickListener getOnRecordClickListener()
	{
		return recordClickListener;
	}

	public void setOnRecordClickListener(RecordClickListener listener)
	{
		recordClickListener = listener;
	}

	public static int getDownedIndex()
	{
		return downedIndex;
	}

	public static void setDownedIndex(int downedIndex)
	{
		DiaryView.downedIndex = downedIndex;
	}

	public static int getClickedIndex()
	{
		return clickedIndex;
	}

	public static void setClickedIndex(int clickedIndex)
	{
		DiaryView.clickedIndex = clickedIndex;
	}

	// утилиты

	private static String timeToStr(Date time)
	{
		return FORMAT_DIARY_TIME_LOC.format(time);
	}

	private static int getTextWidth(String text, Paint paint)
	{
		float[] w = new float[text.length()];
		paint.getTextWidths(text, w);
		float summ = 0;
		for (int i = 0; i < w.length; i++)
		{
			summ += w[i];
		}
		return (int) summ;
	}

	/**
	 * Определяет высоту страницы
	 * 
	 * @param page
	 *            Страница
	 * @return Высота
	 */
	private static int getPageHeight(List<?> page)
	{
		if (null == page)
		{
			throw new NullPointerException("Page can't be null");
		}
		/*
		 * if (null == page) return 2 * BORD; else
		 */
		return (2 * BORD) + (page.size() * REC_HEIGHT);
	}

	/**
	 * Определяет высоту текущей страницы
	 * 
	 * @return Высота
	 */
	private int getPageHeight()
	{
		return getPageHeight(records);
	}

	private static String trimToFit(String str, float space)
	{
		if (space <= 0)
		{
			throw new IllegalArgumentException("Space must be positive! ;)");
		}

		if (getTextWidth(str, paintRec) <= space)
		{
			return str;
		}
		else
		{
			// Log.i(TAG, "trimToFit(" + str + ", "+String.valueOf(space)+")");
			// TODO: оптимизировать
			while ((getTextWidth(str + "...", paintRec) > space) && (str != ""))
			{
				// Log.v(TAG, "str = " + str);
				str = str.substring(0, str.length() - 1);
			}
			return str + "...";
		}
	}

	private int getScreenWidth()
	{
		// сказочный грязный хак!
		if (isInEditMode())
		{
			return 380;
		}
		Context ctx = getContext();
		WindowManager wm = (WindowManager) ctx.getSystemService(Context.WINDOW_SERVICE);

		Point outSize = new Point();
		wm.getDefaultDisplay().getSize(outSize);
		return outSize.x;
	}

	// вспомогательные методы рисования

	/**
	 * Рисует красивый прямоугольник
	 * 
	 * @param canvas
	 *            Где рисовать
	 * @param r
	 *            Что рисовать
	 * @param color
	 *            Каким цветом
	 */
	private static void drawPanelBack(Canvas canvas, RectF r, int color)
	{
		Paint paint = new Paint();

		// рисуем фон
		paint.setStyle(Style.FILL);
		paint.setColor(color);
		canvas.drawRect(r, paint);

		paint.setStyle(Style.STROKE);

		// рисуем тёмную рамку
		paint.setColor(COLOR_PANEL_DARK_BORD);
		canvas.drawRect(r.left + 1, r.top + 1, r.right + 1, r.bottom + 1, paint);

		// рисуем светлую рамку
		paint.setColor(COLOR_PANEL_LIGHT_BORD);
		canvas.drawRect(r, paint);
	}

	/**
	 * Рендеринг страницы
	 * 
	 * @param page
	 *            Страница
	 * @param canvas
	 *            Канва
	 */
	private void drawPage(List<Versioned<DiaryRecord>> records, Canvas canvas)
	{
		if (null == records)
		{
			throw new NullPointerException("Page can't be null");
		}

		Log.d(TAG, String.format("drawPage(): rendering %d items into buffer...", records.size()));

		// TODO: сделать тестирование скорости вывода

		// очистка
		canvas.clipRect(0, 0, screenWidth, getPageHeight(records));
		canvas.drawColor(COLOR_BACKGROUND);

		// заполнение

		RectF r = new RectF();
		r.left = BORD;
		r.right = screenWidth - BORD;
		int top = BORD;

		// String capt = CaptionFmt.format(page.date());
		// canvas.drawText(capt, (screenWidth - getTextWidth(capt,
		// paintCaption)) / 2, top + TEXT_BORD + TEXT_SIZE, paintCaption);
		// top += (TEXT_SIZE + 2*TEXT_BORD);

		for (int i = 0; i < records.size(); i++)
		{
			DiaryRecord rec = records.get(i).getData();

			r.top = top;
			r.bottom = r.top + TEXT_SIZE + (2 * TEXT_BORD);

			if (rec.getClass() == BloodRecord.class)
			{
				BloodRecord temp = (BloodRecord) rec;

				String units = getContext().getString(R.string.common_bs_unit_mmol);
				String finger = temp.getFinger() == -1 ? "" : String.format("(%s)", fingers[temp.getFinger()]);
				String text = String.format(Locale.US, "%.1f %s %s", temp.getValue(), units, finger);

				drawPanelBack(canvas, r, (getClickedIndex() == i ? COLOR_PANEL_BLOOD_SEL : COLOR_PANEL_BLOOD_STD));
				canvas.drawText(timeToStr(temp.getTime()), LEFT_TIME, r.top + TEXT_BORD + TEXT_SIZE, paintTime);
				canvas.drawText(text, LEFT_RECS, r.top + TEXT_BORD + TEXT_SIZE, paintRec);

				top += (TEXT_SIZE + (2 * TEXT_BORD));
			}
			else if (rec.getClass() == InsRecord.class)
			{
				InsRecord temp = (InsRecord) rec;

				drawPanelBack(canvas, r, (getClickedIndex() == i ? COLOR_PANEL_INS_SEL : COLOR_PANEL_INS_STD));
				canvas.drawText(timeToStr(temp.getTime()), LEFT_TIME, r.top + TEXT_BORD + TEXT_SIZE, paintTime);
				canvas.drawText(String.valueOf(temp.getValue()) + " ед", LEFT_RECS, r.top + TEXT_BORD + TEXT_SIZE,
						paintRec);
				top += (TEXT_SIZE + (2 * TEXT_BORD));
			}
			else if (rec.getClass() == MealRecord.class)
			{
				MealRecord temp = (MealRecord) rec;

				/*
				 * if (temp.getShortMeal()) drawPanelBack(canvas, r, Color.YELLOW); else
				 * drawPanelBack(canvas, r, COLOR_PANEL_MEAL_STD);
				 */
				drawPanelBack(canvas, r, (getClickedIndex() == i ? COLOR_PANEL_MEAL_SEL : COLOR_PANEL_MEAL_STD));

				String text = trimToFit(MealFormatter.format(temp, MealFormatter.FormatStyle.MOST_CARBS), r.right
						- LEFT_RECS);
				canvas.drawText(timeToStr(temp.getTime()), LEFT_TIME, r.top + TEXT_BORD + TEXT_SIZE, paintTime);
				canvas.drawText(text, LEFT_RECS, r.top + TEXT_BORD + TEXT_SIZE, paintRec);

				top += (TEXT_SIZE + (2 * TEXT_BORD));
			}
			else if (rec.getClass() == NoteRecord.class)
			{
				NoteRecord temp = (NoteRecord) rec;

				drawPanelBack(canvas, r, (getClickedIndex() == i ? COLOR_PANEL_NOTE_SEL : COLOR_PANEL_NOTE_STD));

				String text = trimToFit(temp.getText(), r.right - LEFT_RECS);
				canvas.drawText(timeToStr(temp.getTime()), LEFT_TIME, r.top + TEXT_BORD + TEXT_SIZE, paintTime);
				canvas.drawText(text, LEFT_RECS, r.top + TEXT_BORD + TEXT_SIZE, paintRec);

				top += (TEXT_SIZE + (2 * TEXT_BORD));
			}
			else
			{
				throw new UnsupportedOperationException("Unsupported record type: " + rec.getClass().getName());
			}
		}
		// canvas.drawText("[" + String.valueOf(page.getVersion()) + "]", r.right - 50, BORD +
		// TEXT_BORD + TEXT_SIZE,
		// paintTime);
	}

	/**
	 * Пересчёт размера буфера и повторный рендеринг страницы
	 */
	void updateBuffer()
	{
		if (null != records)
		{
			// Log.d(TAG, "updateBuffer(): setLayoutParams()");

			int height = getPageHeight(records);

			LayoutParams params = getLayoutParams();
			params.height = height;
			setLayoutParams(params);

			// setMinimumHeight(height);
			// Log.d(TAG, "updateBuffer(): buffers are re-created");
			bufferBitmap = Bitmap.createBitmap(getScreenWidth(), height, Bitmap.Config.ARGB_8888);
			bufferCanvas = new Canvas(bufferBitmap);
			drawPage(records, bufferCanvas);
		}
	}

	// события

	@Override
	protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec)
	{
		super.onMeasure(widthMeasureSpec, heightMeasureSpec);
		// Log.d(TAG, "onMeasure(): dimensions are setted");
		if (!isInEditMode())
		{
			setMeasuredDimension(getScreenWidth(), getPageHeight());
			updateBuffer();
		}
	}

	@Override
	protected void onSizeChanged(int w, int h, int oldw, int oldh)
	{
		Log.i(TAG, "onSizeChanged(): client sizes are updated: w=" + w + "; h = " + h);
		screenWidth = w;
		// updateBuffer();
	}

	@Override
	protected void onDraw(Canvas canvas)
	{
		// Log.v(TAG, "onDraw()");
		super.onDraw(canvas);

		if (!isInEditMode() && (bufferBitmap != null))
		{
			canvas.drawBitmap(bufferBitmap, 0, 0, paintDefault);
		}
	}

	/*
	 * public boolean onTouchEvent(MotionEvent E) {
	 * 
	 * if (E.getAction() == MotionEvent.ACTION_DOWN) { int x = (int) E.getX(); int y = (int)
	 * E.getY(); int index = (y - BORD) / REC_HEIGHT;
	 * 
	 * Log.i(TAG, "onTouchEvent() (" + x + "; " + y + "): " + E.getAction());
	 * 
	 * if ((page != null) && (index >= 0) && (index < page.count())) { DiaryRecord rec =
	 * page.get(index);
	 * 
	 * if (rec.getClass() == BloodRecord.class) { BloodRecord temp = (BloodRecord) rec; Log.i(TAG,
	 * "BloodRec: " + temp.getTime() + ": " + temp.getValue()); } } } return false; }
	 */
}
