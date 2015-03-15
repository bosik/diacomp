package org.bosik.diacomp.web.frontend.wicket.components.diary.blood;

import java.util.Arrays;
import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxFallbackLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.image.Image;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.utils.Utils;

public class DiaryPanelBlood extends Panel
{
	private static final long		serialVersionUID	= 1L;

	IModel<Versioned<BloodRecord>>	model;
	Label							deleteHint;

	@SpringBean
	DiaryService					diaryService;

	public DiaryPanelBlood(String id, IModel<Versioned<BloodRecord>> model)
	{
		super(id);
		this.model = model;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		BloodRecord rec = model.getObject().getData();
		// TODO: i18n
		add(new Image("icon", Model.of("icon.png")).add(AttributeModifier.replace("title", "Замер СК")));
		add(new Label("time", Utils.formatTimeLocalShort(rec.getTime())));
		add(new Label("value", formatBloodValue(rec.getValue())));
		add(new Label("finger", formatBloodFinger(rec.getFinger())).add(AttributeModifier.replace("title",
				formatBloodFingerHint(rec.getFinger()))));

		AjaxFallbackLink<Void> linkDelete = new AjaxFallbackLink<Void>("delete")
		{
			private static final long	serialVersionUID	= -3995475639165455772L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				final Versioned<BloodRecord> object = model.getObject();
				if (!object.isDeleted())
				{
					object.setDeleted(true);
					DiaryPanelBlood.this.add(AttributeModifier.replace("style", "opacity: 0.4"));
					// TODO: i18n
					deleteHint.add(AttributeModifier.replace("title", "Восстановить"));
					target.add(DiaryPanelBlood.this, deleteHint);
				}
				else
				{
					object.setDeleted(false);
					DiaryPanelBlood.this.add(AttributeModifier.replace("style", ""));
					// TODO: i18n
					deleteHint.add(AttributeModifier.replace("title", "Удалить"));
					target.add(DiaryPanelBlood.this, deleteHint);
				}
				object.updateTimeStamp();
				diaryService.save(Arrays.<Versioned<DiaryRecord>> asList(new Versioned<DiaryRecord>(object)));
			}
		};
		add(linkDelete);

		deleteHint = new Label("deleteText", new Model<String>()
		{
			private static final long	serialVersionUID	= 3936985834863628353L;

			@Override
			public String getObject()
			{
				return model.getObject().isDeleted() ? "↶" : "×";
			}
		});
		// TODO: i18n
		deleteHint.add(AttributeModifier.replace("title", "Удалить"));
		deleteHint.setOutputMarkupId(true);
		linkDelete.add(deleteHint);

		setOutputMarkupId(true);
	}

	private static String formatBloodValue(double value)
	{
		return String.format("%.1f ", value) + "ммоль/л";
	}

	private String formatBloodFinger(int index)
	{
		if (index >= 0 && index < 10)
		{
			return getString("finger.short." + index);
		}
		else
		{
			return "";
		}
	}

	private String formatBloodFingerHint(int index)
	{
		if (index >= 0 && index < 10)
		{
			return getString("finger.long." + index);
		}
		else
		{
			return "";
		}
	}
}
