package org.bosik.diacomp.web.frontend.wicket.dialogs.diary.blood;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.model.IModel;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.web.frontend.wicket.dialogs.common.CommonEditor;

public abstract class DiaryEditorBlood extends CommonEditor<BloodRecord>
{
	private static final long	serialVersionUID	= 1L;

	public DiaryEditorBlood(String id)
	{
		super(id);

		setMinimalWidth(420);
		setMinimalHeight(280);

		setInitialWidth(420);
		setInitialHeight(280);

		setTitle(getString("diary.editor.blood.caption"));
	}

	public void show(AjaxRequestTarget target, IModel<Versioned<BloodRecord>> model)
	{
		setContent(new DiaryEditorBloodContentPanel(getContentId(), model)
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			protected void onCancel(AjaxRequestTarget target)
			{
				DiaryEditorBlood.this.onCancel(target);
			}

			@Override
			protected void onSave(AjaxRequestTarget target, IModel<Versioned<BloodRecord>> model)
			{
				DiaryEditorBlood.this.onSave(target, model);
			}
		});
		super.show(target);
	}
}
