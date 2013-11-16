package org.bosik.compensation.face.views;

import org.bosik.compensation.face.R;
import org.bosik.compensation.persistence.entity.diary.DiaryPage;
import org.bosik.compensation.persistence.entity.diary.records.BloodRecord;
import org.bosik.compensation.persistence.entity.diary.records.DiaryRecord;
import org.bosik.compensation.persistence.entity.diary.records.InsRecord;
import org.bosik.compensation.persistence.entity.diary.records.MealRecord;
import org.bosik.compensation.persistence.entity.diary.records.NoteRecord;
import org.bosik.compensation.utils.Utils;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Paint.Style;
import android.graphics.RectF;
import android.graphics.Typeface;
import android.util.AttributeSet;
import android.util.Log;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup.LayoutParams;
import android.view.WindowManager;

public class DiaryView extends View implements OnClickListener, View.OnTouchListener
{
	// отладочная печать
	private static final String	TAG						= "DiaryView";

	// стили рисования
	private static final Paint	paintNoPage				= new Paint();
	private static final Paint	paintCaption			= new Paint();
	private static final Paint	paintTime				= new Paint();
	private static final Paint	paintRec				= new Paint();
	private static final Paint	paintDefault			= new Paint();

	// отступы
	private static final int	BORD					= 12;
	private static final int	TEXT_SIZE				= 24;
	private static final int	TEXT_NOPAGE_SIZE		= 32;
	private static final int	TEXT_BORD				= 10;
	private static int			LEFT_TIME;
	private static int			LEFT_RECS;
	private static final int	REC_HEIGHT;

	// цвета
	private static final int	COLOR_PANEL_LIGHT_BORD	= Color.WHITE;
	private static final int	COLOR_PANEL_DARK_BORD	= Color.GRAY;
	private static final int	COLOR_PANEL_BLOOD_STD	= Color.rgb(230, 238, 255);
	private static final int	COLOR_PANEL_BLOOD_SEL	= Color.rgb(204, 221, 247);
	private static final int	COLOR_PANEL_INS_STD		= Color.WHITE;
	private static final int	COLOR_PANEL_INS_SEL		= Color.rgb(240, 240, 240);
	private static final int	COLOR_PANEL_NOTE_STD	= Color.rgb(216, 255, 228);
	private static final int	COLOR_PANEL_NOTE_SEL	= Color.rgb(179, 255, 202);
	private static final int	COLOR_PANEL_MEAL_STD	= Color.rgb(255, 255, 221);
	private static final int	COLOR_PANEL_MEAL_SEL	= Color.rgb(255, 255, 153);
	private static final int	COLOR_BACKGROUND		= Color.WHITE;

	// поля

	// private static final String TEXT_NOPAGE = "Страница пуста";
	private String[]			fingers;
	private int					screenWidth				= getScreenWidth();
	private DiaryPage			page					= null;
	private Bitmap				bufferBitmap;
	private Canvas				bufferCanvas;
	private int					clickedX				= -1;
	private int					clickedY				= -1;
	private static int			downedIndex				= -1;
	private static int			clickedIndex			= -1;
	private RecordClickListener	recordClickListener;

	// инициализация
	static
	{
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
		LEFT_RECS = BORD + (2 * TEXT_BORD) + getTextWidth("12:34", paintTime);
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
		setOnClickListener(this);
		setOnTouchListener(this);
		setClickable(true);
		Log.e(TAG, "clickListener setuped OK");
	}

	/**
	 * Устанавливает страницу для просмотра и редактирования
	 * 
	 * @param page
	 *            Страница
	 */
	public void setPage(DiaryPage page)
	{
		// Log.i(TAG, "setPage()");
		if (null == page)
		{
			throw new NullPointerException("Page can't be null");
		}

		if ((this.page == null) || (page.getDate() != this.page.getDate()))
		{
			setDownedIndex(-1);
			setClickedIndex(-1);
		}

		/**
		 * Нам необходимо сохранить страницу, чтобы иметь возможность перерисовать её при смене
		 * ориентации экрана
		 */
		this.page = page;

		Log.d(TAG, "setPage(): dimensions are setted");
		setMeasuredDimension(getScreenWidth(), getPageHeight(this.page));

		updateBuffer();

		Log.d(TAG, "setPage(): invalidating");
		invalidate();

		Log.d(TAG, "setPage(): setMinimumHeight()");
		setMinimumHeight(getPageHeight(this.page));
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

	public DiaryPage getPage()
	{
		return page;
	}

	// утилиты

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
	private static int getPageHeight(DiaryPage page)
	{
		if (null == page)
		{
			throw new NullPointerException("Page can't be null");
		}
		/*
		 * if (null == page) return 2 * BORD; else
		 */
		return (2 * BORD) + (page.count() * REC_HEIGHT);
	}

	/**
	 * Определяет высоту текущей страницы
	 * 
	 * @return Высота
	 */
	private int getPageHeight()
	{
		return getPageHeight(page);
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
		return wm.getDefaultDisplay().getWidth();
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
	private void drawPanelBack(Canvas canvas, RectF r, int color)
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
	private void drawPage(DiaryPage page, Canvas canvas)
	{
		// Log.d(TAG, "drawPage(): page is rendering into buffer...");

		if (null == page)
		{
			throw new IllegalArgumentException("Page can't be null");
		}

		// TODO: сделать тестирование скорости вывода

		// очистка
		canvas.clipRect(0, 0, screenWidth, getPageHeight(page));
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

		for (int i = 0; i < page.count(); i++)
		{
			// Log.d(TAG, "drawPage():извлечение записи №" + i);
			DiaryRecord rec = page.get(i);

			r.top = top;
			r.bottom = r.top + TEXT_SIZE + (2 * TEXT_BORD);

			if (rec.getClass() == BloodRecord.class)
			{
				BloodRecord temp = (BloodRecord) rec;

				drawPanelBack(canvas, r, (getClickedIndex() == i ? COLOR_PANEL_BLOOD_SEL : COLOR_PANEL_BLOOD_STD));

				String finger = temp.getFinger() == -1 ? "" : " | " + fingers[temp.getFinger()];

				canvas.drawText(Utils.timeToStr(temp.getTime()), LEFT_TIME, r.top + TEXT_BORD + TEXT_SIZE, paintTime);
				canvas.drawText(String.valueOf(temp.getValue()) + finger, LEFT_RECS, r.top + TEXT_BORD + TEXT_SIZE,
						paintRec);

				top += (TEXT_SIZE + (2 * TEXT_BORD));
			}
			else
				if (rec.getClass() == InsRecord.class)
				{
					InsRecord temp = (InsRecord) rec;

					drawPanelBack(canvas, r, (getClickedIndex() == i ? COLOR_PANEL_INS_SEL : COLOR_PANEL_INS_STD));
					canvas.drawText(Utils.timeToStr(temp.getTime()), LEFT_TIME, r.top + TEXT_BORD + TEXT_SIZE,
							paintTime);
					canvas.drawText(String.valueOf(temp.getValue()) + " ед", LEFT_RECS, r.top + TEXT_BORD + TEXT_SIZE,
							paintRec);
					top += (TEXT_SIZE + (2 * TEXT_BORD));
				}
				else
					if (rec.getClass() == MealRecord.class)
					{
						MealRecord temp = (MealRecord) rec;

						/*
						 * if (temp.getShortMeal()) drawPanelBack(canvas, r, Color.YELLOW); else
						 * drawPanelBack(canvas, r, COLOR_PANEL_MEAL_STD);
						 */
						drawPanelBack(canvas, r, (getClickedIndex() == i ? COLOR_PANEL_MEAL_SEL : COLOR_PANEL_MEAL_STD));

						Log.v(TAG, "drawPage(): r.right = " + r.right);
						Log.v(TAG, "drawPage(): LEFT_RECS = " + LEFT_RECS);

						String text = trimToFit(MealFormatter.format(temp, MealFormatter.FormatStyle.MOST_CARBS),
								r.right - LEFT_RECS);
						canvas.drawText(Utils.timeToStr(temp.getTime()), LEFT_TIME, r.top + TEXT_BORD + TEXT_SIZE,
								paintTime);
						canvas.drawText(text, LEFT_RECS, r.top + TEXT_BORD + TEXT_SIZE, paintRec);

						top += (TEXT_SIZE + (2 * TEXT_BORD));
					}
					else
						if (rec.getClass() == NoteRecord.class)
						{
							NoteRecord temp = (NoteRecord) rec;

							drawPanelBack(canvas, r, (getClickedIndex() == i ? COLOR_PANEL_NOTE_SEL
									: COLOR_PANEL_NOTE_STD));

							String text = trimToFit(temp.getText(), r.right - LEFT_RECS);
							canvas.drawText(Utils.timeToStr(temp.getTime()), LEFT_TIME, r.top + TEXT_BORD + TEXT_SIZE,
									paintTime);
							canvas.drawText(text, LEFT_RECS, r.top + TEXT_BORD + TEXT_SIZE, paintRec);

							top += (TEXT_SIZE + (2 * TEXT_BORD));
						}
						else
						{
							throw new UnsupportedOperationException("Unsupported record type: "
									+ rec.getClass().getName());
						}
		}
		canvas.drawText("[" + String.valueOf(page.getVersion()) + "]", r.right - 50, BORD + TEXT_BORD + TEXT_SIZE,
				paintTime);
	}

	/**
	 * Пересчёт размера буфера и повторный рендеринг страницы
	 */
	private void updateBuffer()
	{
		if (null != page)
		{
			// Log.d(TAG, "updateBuffer(): setLayoutParams()");

			int height = getPageHeight(page);

			LayoutParams params = getLayoutParams();
			params.height = height;
			setLayoutParams(params);

			// setMinimumHeight(height);
			// Log.d(TAG, "updateBuffer(): buffers are re-created");
			bufferBitmap = Bitmap.createBitmap(getScreenWidth(), height, Bitmap.Config.ARGB_8888);
			bufferCanvas = new Canvas(bufferBitmap);
			drawPage(page, bufferCanvas);
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

	@Override
	public void onClick(View v)
	{
		Log.e(TAG, "onClick()");

		setClickedIndex(getDownedIndex());
		updateBuffer();
		invalidate();

		if ((page != null) && (getDownedIndex() >= 0) && (getDownedIndex() < page.count()))
		{
			if (recordClickListener != null)
			{
				recordClickListener.onRecordClick(getDownedIndex());
			}
		}
	}

	@Override
	public boolean onTouch(View v, MotionEvent E)
	{
		if (E.getAction() == MotionEvent.ACTION_DOWN)
		{
			clickedX = (int) E.getX();
			clickedY = (int) E.getY();
			setDownedIndex((clickedY - BORD) / REC_HEIGHT);

			updateBuffer();
			invalidate();
		}
		return false;
	}

}
