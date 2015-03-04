package org.bosik.diacomp.web.backend.features.base.dish;

import org.bosik.diacomp.core.test.fakes.services.FakeDishBaseService;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

@Service
@Profile("fake")
public class FakeDishSpringService extends FakeDishBaseService
{

}
