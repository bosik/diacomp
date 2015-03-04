package org.bosik.diacomp.web.backend.features.base.food;

import org.bosik.diacomp.core.test.fakes.services.FakeFoodBaseService;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

@Service
@Profile("fake")
public class FakeFoodSpringService extends FakeFoodBaseService
{

}
